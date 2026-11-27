module Devm.Server.Environment where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Devm.Server.Environment.Internal qualified as Internal
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

type HandlerT :: (Type -> Type) -> (Type -> Type)
type HandlerT = Internal.HandlerT

mapHandlerT :: (m a -> n b) -> HandlerT m a -> HandlerT n b
mapHandlerT f = Internal.MkHandlerT . mapReaderT f . Internal.unHandlerT

getVMState :: Monad m => HandlerT m (STM.TVar VMPool)
getVMState = Internal.vms <$> Internal.getEnvironment

getVMExecutable :: Monad m => VMID -> HandlerT m (Maybe VMExecutable)
getVMExecutable identifier = do
  MkVMExecutables executables <- Internal.executables <$> Internal.getEnvironment
  pure $ Map.lookup identifier executables

getLogRequests :: Monad m => HandlerT m Bool
getLogRequests = Internal.logRequests <$> Internal.getEnvironment

getLogVMs :: Monad m => HandlerT m Bool
getLogVMs = Internal.logVMs <$> Internal.getEnvironment

getCommandTimeoutDefault :: Monad m => HandlerT m Int
getCommandTimeoutDefault = Internal.commandTimeoutDefault <$> Internal.getEnvironment

getCommandTimeoutMax :: Monad m => HandlerT m Int
getCommandTimeoutMax = Internal.commandTimeoutMax <$> Internal.getEnvironment

invalidAccess :: MonadIO m => HandlerT m MCP.Server.Content
invalidAccess = do
  lift $ liftIO $ Concurrent.threadDelay 200000
  pure $ MCP.Server.ContentText "Error: Access token is invalid. There is no running VM, that can be accessed with that token."

type SSHProcessResult :: Type
data SSHProcessResult
  = SSHFinished Exit.ExitCode String String
  | SSHTimedOut

commandTerminationGracePeriod :: Int
commandTerminationGracePeriod = 5_000_000

sshCommand :: MonadIO m => VMID -> Text.Text -> Int -> HandlerT m Text.Text
sshCommand identifier command timeoutSeconds = do
  sshKeyFile <- Internal.sshKeyFile <$> Internal.getEnvironment
  let
    address :: String
    address = "10.14.2" <> Text.unpack (displayVMID identifier) <> ".100"

    sshOptions :: [String]
    sshOptions =
      [ "-o"
      , "HostName=" <> address
      , "-o"
      , "User=dev"
      , "-o"
      , "ConnectTimeout=10"
      , "-o"
      , "StrictHostKeyChecking=no"
      , "-o"
      , "UserKnownHostsFile=/dev/null"
      , "-o"
      , "LogLevel=ERROR"
      , "-o"
      , "IdentityFile=" <> sshKeyFile
      , "-o"
      , "IdentitiesOnly=yes"
      , "-o"
      , "PreferredAuthentications=publickey"
      , "-o"
      , "PubkeyAuthentication=yes"
      , "-o"
      , "PasswordAuthentication=no"
      , "-o"
      , "BatchMode=yes"
      , "-T"
      ]

    sshArguments :: [String]
    sshArguments = sshOptions <> ["dev@devm", "--", Text.unpack command]

  result <-
    lift $
      liftIO $
        Exception.catch
          (runSSHProcess sshArguments (timeoutSeconds * 1_000_000))
          (pure . sshException)
  pure $ formatResult result
 where
  sshException :: Exception.IOException -> SSHProcessResult
  sshException err = SSHFinished (Exit.ExitFailure 1) "" $ "Could not run ssh: " <> show err

  formatResult :: SSHProcessResult -> Text.Text
  formatResult SSHTimedOut =
    "Error: Command timed out."
  formatResult (SSHFinished exitCode output errorOutput) =
    Text.pack $
      case exitCode of
        Exit.ExitSuccess ->
          output <> if null errorOutput then "" else "stderr:\n" <> errorOutput
        Exit.ExitFailure code ->
          "Exit code " <> show code <> "\n" <> output <> if null errorOutput then "" else "stderr:\n" <> errorOutput

runSSHProcess :: [String] -> Int -> IO SSHProcessResult
runSSHProcess sshArguments timeoutMicroseconds = do
  created <-
    Process.createProcess $
      (Process.proc "ssh" sshArguments)
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
  case created of
    (_, Just stdoutHandle, Just stderrHandle, processHandle) -> do
      stdoutVar <- Concurrent.newEmptyMVar
      stderrVar <- Concurrent.newEmptyMVar
      _ <- Concurrent.forkIO $ Concurrent.putMVar stdoutVar =<< readHandle stdoutHandle
      _ <- Concurrent.forkIO $ Concurrent.putMVar stderrVar =<< readHandle stderrHandle
      exited <- VMProcess.waitForProcessTimeout timeoutMicroseconds processHandle
      timedOut <-
        if exited
          then pure False
          else do
            VMProcess.terminateProcessHandle commandTerminationGracePeriod "Command" processHandle
            pure True
      output <- MVar.takeMVar stdoutVar
      errorOutput <- MVar.takeMVar stderrVar
      exitCode <- Process.waitForProcess processHandle
      if timedOut
        then pure SSHTimedOut
        else pure $ SSHFinished exitCode output errorOutput
    _ ->
      Exception.throwIO $ IOError.userError "Could not create pipes for ssh."

readHandle :: IO.Handle -> IO String
readHandle handle = readAll `Exception.catch` onError
 where
  readAll :: IO String
  readAll = do
    contents <- IO.hGetContents handle
    _ <- Exception.evaluate (length contents)
    pure contents

  onError :: Exception.IOException -> IO String
  onError err =
    pure $ "Error reading command output: " <> show err
