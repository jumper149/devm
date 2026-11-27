module Devm.Server.Environment where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.ByteString qualified as ByteString
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
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
  = SSHFinished Exit.ExitCode Text.Text Text.Text
  | SSHTimedOut
  | SSHOutputTooLarge Text.Text Text.Text
  | SSHOutputError Text.Text

type SSHWaitResult :: Type
data SSHWaitResult
  = SSHWaitExited
  | SSHWaitTimedOut
  | SSHWaitOutputOverflow

commandTerminationGracePeriod :: Int
commandTerminationGracePeriod = 5_000_000

commandOutputMax :: Int
commandOutputMax = 1_048_576

outputChunkSize :: Int
outputChunkSize = 32_768

commandPollInterval :: Int
commandPollInterval = 10_000

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
  sshException err = SSHFinished (Exit.ExitFailure 1) "" $ "Could not run ssh: " <> Text.pack (show err)

  formatResult :: SSHProcessResult -> Text.Text
  formatResult SSHTimedOut =
    "Error: Command timed out."
  formatResult (SSHOutputTooLarge output errorOutput) =
    "Error: Command output is too large. Maximum output is "
      <> Text.pack (show commandOutputMax)
      <> " bytes.\nTruncated output:\n"
      <> output
      <> stderrPart errorOutput
  formatResult (SSHOutputError message) =
    message
  formatResult (SSHFinished exitCode output errorOutput) =
    case exitCode of
      Exit.ExitSuccess ->
        output <> stderrPart errorOutput
      Exit.ExitFailure code ->
        "Exit code " <> Text.pack (show code) <> "\n" <> output <> stderrPart errorOutput

stderrPart :: Text.Text -> Text.Text
stderrPart errorOutput =
  if Text.null errorOutput
    then ""
    else "stderr:\n" <> errorOutput

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
      IO.hSetBinaryMode stdoutHandle True
      IO.hSetBinaryMode stderrHandle True
      remainingVar <- STM.newTVarIO commandOutputMax
      overflowVar <- STM.newTVarIO False
      stdoutVar <- Concurrent.newEmptyMVar
      stderrVar <- Concurrent.newEmptyMVar
      _ <- Concurrent.forkIO $ Concurrent.putMVar stdoutVar =<< readBoundedOutput remainingVar overflowVar stdoutHandle
      _ <- Concurrent.forkIO $ Concurrent.putMVar stderrVar =<< readBoundedOutput remainingVar overflowVar stderrHandle
      waitResult <- waitForSSHProcess timeoutMicroseconds overflowVar processHandle
      case waitResult of
        SSHWaitExited ->
          pure ()
        SSHWaitTimedOut ->
          VMProcess.terminateProcessHandle commandTerminationGracePeriod "Command" processHandle
        SSHWaitOutputOverflow ->
          VMProcess.terminateProcessHandle commandTerminationGracePeriod "Command" processHandle
      stdoutResult <- MVar.takeMVar stdoutVar
      stderrResult <- MVar.takeMVar stderrVar
      exitCode <- Process.waitForProcess processHandle
      finalOverflow <- STM.readTVarIO overflowVar
      case decodeOutputPair stdoutResult stderrResult of
        Left err ->
          pure $ SSHOutputError err
        Right (output, errorOutput) ->
          if finalOverflow
            then pure $ SSHOutputTooLarge output errorOutput
            else case waitResult of
              SSHWaitTimedOut ->
                pure SSHTimedOut
              SSHWaitOutputOverflow ->
                pure $ SSHOutputTooLarge output errorOutput
              SSHWaitExited ->
                pure $ SSHFinished exitCode output errorOutput
    _ ->
      Exception.throwIO $ IOError.userError "Could not create pipes for ssh."

waitForSSHProcess :: Int -> STM.TVar Bool -> Process.ProcessHandle -> IO SSHWaitResult
waitForSSHProcess maxWait overflowVar processHandle = go 0
 where
  go :: Int -> IO SSHWaitResult
  go elapsed = do
    overflow <- STM.readTVarIO overflowVar
    if overflow
      then pure SSHWaitOutputOverflow
      else
        if elapsed >= maxWait
          then pure SSHWaitTimedOut
          else do
            exited <- Process.getProcessExitCode processHandle
            case exited of
              Just _ ->
                pure SSHWaitExited
              Nothing -> do
                Concurrent.threadDelay commandPollInterval
                go (elapsed + commandPollInterval)

readBoundedOutput :: STM.TVar Int -> STM.TVar Bool -> IO.Handle -> IO (Either String ByteString.ByteString)
readBoundedOutput remainingVar overflowVar handle = go [] `Exception.catch` onError
 where
  go :: [ByteString.ByteString] -> IO (Either String ByteString.ByteString)
  go chunks = do
    chunk <- ByteString.hGetSome handle outputChunkSize
    if ByteString.null chunk
      then pure $ Right $ ByteString.concat $ reverse chunks
      else do
        exceeded <- STM.atomically $ reserveOutput remainingVar overflowVar chunk
        if exceeded
          then pure $ Right $ ByteString.concat $ reverse chunks
          else go (chunk : chunks)

  onError :: Exception.IOException -> IO (Either String ByteString.ByteString)
  onError err =
    pure $ Left $ "Error reading command output: " <> show err

reserveOutput :: STM.TVar Int -> STM.TVar Bool -> ByteString.ByteString -> STM.STM Bool
reserveOutput remainingVar overflowVar chunk = do
  remaining <- STM.readTVar remainingVar
  let chunkSize = ByteString.length chunk
  if chunkSize <= remaining
    then do
      STM.writeTVar remainingVar (remaining - chunkSize)
      pure False
    else do
      STM.writeTVar remainingVar 0
      STM.writeTVar overflowVar True
      pure True

decodeOutputPair :: Either String ByteString.ByteString -> Either String ByteString.ByteString -> Either Text.Text (Text.Text, Text.Text)
decodeOutputPair stdoutResult stderrResult =
  case decodeOutput stdoutResult of
    Left err ->
      Left $ Text.pack err
    Right stdout ->
      case decodeOutput stderrResult of
        Left err ->
          Left $ Text.pack err
        Right stderr ->
          Right (stdout, stderr)

decodeOutput :: Either String ByteString.ByteString -> Either String Text.Text
decodeOutput (Left err) =
  Left err
decodeOutput (Right bytes) =
  Right $ Text.Encoding.decodeUtf8Lenient bytes
