{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified
import System.IO qualified as IO

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , command :: Text.Text
  }

handleCommand :: HandlerEnvironment -> RequestCommand -> IO MCP.Server.Content
handleCommand MkHandlerEnvironment {vms} MkRequestCommand {identifier, command} = do
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
      pure $ MCP.Server.ContentText "Commanded"
