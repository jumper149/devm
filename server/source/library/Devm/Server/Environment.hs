module Devm.Server.Environment where

import Control.Concurrent qualified as Concurrent
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
import MCP.Server qualified
import System.Exit qualified as Exit
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

invalidAccess :: MonadIO m => HandlerT m MCP.Server.Content
invalidAccess = do
  lift $ liftIO $ Concurrent.threadDelay 200000
  pure $ MCP.Server.ContentText "Error: Access token is invalid. There is no running VM, that can be accessed with that token."

sshCommand :: MonadIO m => VMID -> Text.Text -> HandlerT m Text.Text
sshCommand identifier command = do
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
          (Process.readProcessWithExitCode "ssh" sshArguments "")
          ( \err -> do
              let message = "Could not run ssh: " <> show (err :: Exception.IOException)
              pure (Exit.ExitFailure 1, "", message)
          )
  pure $ formatResult result
 where
  formatResult :: (Exit.ExitCode, String, String) -> Text.Text
  formatResult (exitCode, output, errorOutput) =
    Text.pack $
      case exitCode of
        Exit.ExitSuccess ->
          output <> if null errorOutput then "" else "stderr:\n" <> errorOutput
        Exit.ExitFailure code ->
          "Exit code " <> show code <> "\n" <> output <> if null errorOutput then "" else "stderr:\n" <> errorOutput
