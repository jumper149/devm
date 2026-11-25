module Devm.Server.Environment where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Devm.Server.VM
import System.Exit qualified as Exit
import System.Process qualified as Process

type HandlerEnvironment :: Type
data HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
  , executables :: VMExecutables
  , readyMarker :: Text.Text
  , sshKeyFile :: FilePath
  }

type HandlerM :: Type -> Type
newtype HandlerM a = MkHandlerM {unHandlerM :: ReaderT HandlerEnvironment IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runHandlerM :: HandlerEnvironment -> HandlerM a -> IO a
runHandlerM env MkHandlerM {unHandlerM} = runReaderT unHandlerM env

getEnvironment :: HandlerM HandlerEnvironment
getEnvironment = MkHandlerM ask

getVMExecutable :: VMID -> HandlerM (Maybe VMExecutable)
getVMExecutable identifier = do
  MkVMExecutables executables <- executables <$> getEnvironment
  pure $ Map.lookup identifier executables

sshCommand :: VMID -> Text.Text -> HandlerM Text.Text
sshCommand identifier command = do
  sshKeyFile <- sshKeyFile <$> getEnvironment
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
