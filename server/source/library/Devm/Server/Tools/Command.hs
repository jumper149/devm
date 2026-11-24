{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified
import System.Exit qualified as Exit
import System.Process qualified as Process

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , command :: Text.Text
  }

handleCommand :: HandlerEnvironment -> RequestCommand -> IO MCP.Server.Content
handleCommand MkHandlerEnvironment {vms, sshKey} MkRequestCommand {identifier, command} = do
  inUse <- STM.atomically $ do
    pool <- STM.readTVar vms
    pure $
      case lookupVMLock pool identifier of
        Just (VMisAlreadyInUse _) -> True
        _ -> False
  if not inUse
    then pure $ MCP.Server.ContentText "ID is not in use."
    else do
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
          , "IdentityFile=" <> sshKey
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
        Exception.catch
          (Process.readProcessWithExitCode "ssh" sshArguments "")
          ( \err -> do
              let message = "Could not run ssh: " <> show (err :: Exception.IOException)
              pure (Exit.ExitFailure 1, "", message)
          )
      pure $ MCP.Server.ContentText $ formatResult result

formatResult :: (Exit.ExitCode, String, String) -> Text.Text
formatResult (exitCode, output, errorOutput) =
  Text.pack $
    case exitCode of
      Exit.ExitSuccess ->
        output <> if null errorOutput then "" else "stderr:\n" <> errorOutput
      Exit.ExitFailure code ->
        "Exit code " <> show code <> "\n" <> output <> if null errorOutput then "" else "stderr:\n" <> errorOutput
