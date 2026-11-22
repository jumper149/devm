{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Main where

import Control.Concurrent.STM qualified as STM
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import MCP.Server qualified
import MCP.Server.Derive qualified
import Options.Applicative qualified as Opt

type VMID :: Type
data VMID = MkVMID Int
  deriving stock (Eq, Ord)

displayVMID :: VMID -> Text.Text
displayVMID (MkVMID vmID) = Text.pack $ show vmID

type VMLock :: Type
data VMLock = VMisFreeToUse | VMisAlreadyInUse

type VMExecutable :: Type
data VMExecutable = MkVMExecutable Text.Text

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

lockVM :: VMID -> VMPool -> VMPool
lockVM identifier (MkVMPool pool) = MkVMPool $ Map.insert identifier VMisAlreadyInUse pool

unlockVM :: VMID -> VMPool -> VMPool
unlockVM identifier (MkVMPool pool) = MkVMPool $ Map.insert identifier VMisFreeToUse pool

type HandlerEnvironment :: Type
newtype HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
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
handleTool MkHandlerEnvironment {vms} _ctx = \case
  Create MkRequestCreate {identifier} -> do
    STM.atomically $ do
      pool <- STM.readTVar vms
      case lookupVMLock pool identifier of
        Nothing ->
          pure $ MCP.Server.ContentText "ID does not exist."
        Just VMisFreeToUse -> do
          STM.writeTVar vms $ lockVM identifier pool
          pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
        Just VMisAlreadyInUse ->
          pure $ MCP.Server.ContentText "ID is already in use."
  Command MkRequestCommand {} -> pure $ MCP.Server.ContentText "Commanded"
  Destroy MkRequestDestroy {} -> pure $ MCP.Server.ContentText "Destroyed"

info :: MCP.Server.McpServerInfo
info =
  MCP.Server.McpServerInfo
    { MCP.Server.serverName = "devm"
    , MCP.Server.serverVersion = "0"
    , MCP.Server.serverInstructions = ""
    }

type CommandLine :: Type
data CommandLine = MkCommandLine
  { vmExecutables :: Map.Map VMID VMExecutable
  }

commandLineParser :: Opt.Parser CommandLine
commandLineParser =
  MkCommandLine . Map.fromList . zip (MkVMID <$> [1 ..])
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
  MkCommandLine {vmExecutables} <- Opt.execParser commandLineParserInfo
  vms <- initializeVMPool $ Map.keys vmExecutables
  let environment = MkHandlerEnvironment {..}
  let tool = handleTool environment
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttp info handlers
