{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Main where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Unique qualified as Unique
import MCP.Server qualified
import MCP.Server.Derive qualified
import Options.Applicative qualified as Opt
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

type VMID :: Type
data VMID = MkVMID Int
  deriving stock (Eq, Ord)

displayVMID :: VMID -> Text.Text
displayVMID (MkVMID vmID) = Text.pack $ show vmID

type RunningVM :: Type
data RunningVM = MkRunningVM
  { vmToken :: Unique.Unique
  , vmProcess :: Process.ProcessHandle
  , vmInput :: IO.Handle
  , vmOutput :: IO.Handle
  , vmError :: IO.Handle
  }

runningToken :: RunningVM -> Unique.Unique
runningToken MkRunningVM {vmToken} = vmToken

runningProcess :: RunningVM -> Process.ProcessHandle
runningProcess MkRunningVM {vmProcess} = vmProcess

runningInput :: RunningVM -> IO.Handle
runningInput MkRunningVM {vmInput} = vmInput

runningOutput :: RunningVM -> IO.Handle
runningOutput MkRunningVM {vmOutput} = vmOutput

runningError :: RunningVM -> IO.Handle
runningError MkRunningVM {vmError} = vmError

type VMLock :: Type
data VMLock = VMisFreeToUse | VMisStarting | VMisAlreadyInUse RunningVM

type VMExecutable :: Type
data VMExecutable = MkVMExecutable Text.Text

type VMExecutables :: Type
data VMExecutables = MkVMExecutables {unVMExecutables :: Map.Map VMID VMExecutable}

type VMPool :: Type
newtype VMPool = MkVMPool {unVMPool :: Map.Map VMID VMLock}

initializeVMPool :: [VMID] -> IO (STM.TVar VMPool)
initializeVMPool identifiers =
  STM.atomically $
    STM.newTVar $
      MkVMPool $
        Map.fromList $
          (,VMisFreeToUse) <$> identifiers

lookupVMLock :: VMPool -> VMID -> Maybe VMLock
lookupVMLock MkVMPool {unVMPool} identifier =
  Map.lookup identifier unVMPool

setVMLock :: VMID -> VMLock -> VMPool -> VMPool
setVMLock identifier lock (MkVMPool pool) =
  MkVMPool $ Map.insert identifier lock pool

unlockVM :: VMID -> VMPool -> VMPool
unlockVM identifier (MkVMPool pool) =
  MkVMPool $ Map.insert identifier VMisFreeToUse pool

startVM :: VMExecutable -> IO RunningVM
startVM (MkVMExecutable path) = do
  token <- Unique.newUnique
  created <-
    Process.createProcess $
      (Process.proc (Text.unpack path) [])
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
  case created of
    (Just vmInput, Just vmOutput, Just vmError, vmProcess) ->
      pure MkRunningVM {vmToken = token, vmProcess, vmInput, vmOutput, vmError}
    _ ->
      Exception.throwIO $ IOError.userError "Could not create pipes for the VM executable."

monitorVM :: STM.TVar VMPool -> VMID -> RunningVM -> IO ()
monitorVM vms identifier running = do
  _ <- Process.waitForProcess (runningProcess running)
  IO.hClose (runningInput running)
  IO.hClose (runningOutput running)
  IO.hClose (runningError running)
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse current)
        | runningToken current == runningToken running ->
            STM.writeTVar vms $ unlockVM identifier pool
      _ ->
        pure ()

type HandlerEnvironment :: Type
data HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
  , executables :: VMExecutables
  }

type RequestCreate :: Type
data RequestCreate = MkRequestCreate
  { identifier :: VMID
  }

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , command :: Text.Text
  }

type RequestDestroy :: Type
data RequestDestroy = MkRequestDestroy
  { identifier :: VMID
  , destroy :: Text.Text
  }

type Tool :: Type
data Tool
  = Create RequestCreate
  | Command RequestCommand
  | Destroy RequestDestroy

handleTool :: HandlerEnvironment -> MCP.Server.ClientContext -> Tool -> IO MCP.Server.Content
handleTool MkHandlerEnvironment {vms, executables = MkVMExecutables {unVMExecutables = executableMap}} _ctx = \case
  Create MkRequestCreate {identifier} ->
    case Map.lookup identifier executableMap of
      Nothing ->
        pure $ MCP.Server.ContentText "ID does not exist."
      Just executable -> do
        reserved <- STM.atomically $ do
          pool <- STM.readTVar vms
          case lookupVMLock pool identifier of
            Just VMisFreeToUse -> do
              STM.writeTVar vms $ setVMLock identifier VMisStarting pool
              pure True
            _ ->
              pure False
        if not reserved
          then pure $ MCP.Server.ContentText "ID is already in use."
          else do
            running <-
              startVM executable
                `Exception.onException` STM.atomically (STM.modifyTVar' vms (unlockVM identifier))
            STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
            _ <- Concurrent.forkIO $ monitorVM vms identifier running
            pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
  Command MkRequestCommand {identifier, command} -> do
    mRunning <- STM.atomically $ do
      pool <- STM.readTVar vms
      pure $
        case lookupVMLock pool identifier of
          Just (VMisAlreadyInUse running) -> Just running
          _ -> Nothing
    case mRunning of
      Nothing ->
        pure $ MCP.Server.ContentText "ID is not in use."
      Just running -> do
        IO.hPutStrLn (runningInput running) (Text.unpack command)
        IO.hFlush (runningInput running)
        response <- IO.hGetLine (runningOutput running)
        pure $ MCP.Server.ContentText $ Text.pack response
  Destroy MkRequestDestroy {identifier} -> do
    mRunning <- STM.atomically $ do
      pool <- STM.readTVar vms
      case lookupVMLock pool identifier of
        Just (VMisAlreadyInUse running) -> do
          STM.writeTVar vms $ unlockVM identifier pool
          pure $ Just running
        _ ->
          pure Nothing
    case mRunning of
      Nothing ->
        pure $ MCP.Server.ContentText "ID is not in use."
      Just running -> do
        Process.terminateProcess (runningProcess running)
        IO.hClose (runningInput running)
        IO.hClose (runningOutput running)
        IO.hClose (runningError running)
        pure $ MCP.Server.ContentText "Destroyed"

info :: MCP.Server.McpServerInfo
info =
  MCP.Server.McpServerInfo
    { MCP.Server.serverName = "devm"
    , MCP.Server.serverVersion = "0"
    , MCP.Server.serverInstructions = ""
    }

type CommandLine :: Type
data CommandLine = MkCommandLine
  { cliVMExecutables :: VMExecutables
  }

commandLineParser :: Opt.Parser CommandLine
commandLineParser =
  MkCommandLine . MkVMExecutables . Map.fromList . zip (MkVMID <$> [1 ..])
    <$> Opt.some
      ( MkVMExecutable
          <$> Opt.strOption
            ( Opt.long "vm"
                <> Opt.metavar "EXECUTABLE"
                <> Opt.help "Path to the executable of a VM. Repeat once for each VM."
            )
      )

commandLineParserInfo :: Opt.ParserInfo CommandLine
commandLineParserInfo =
  Opt.info (Opt.helper <*> commandLineParser) $
    Opt.fullDesc
      <> Opt.progDesc "Run the devm MCP server."
      <> Opt.header "devm-server"

$(pure [])

main :: IO ()
main = do
  MkCommandLine {cliVMExecutables = executables@(MkVMExecutables {unVMExecutables = executablesMap})} <- Opt.execParser commandLineParserInfo
  vms <- initializeVMPool $ Map.keys executablesMap
  let environment = MkHandlerEnvironment {..}
  let tool = handleTool environment
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttp info handlers
