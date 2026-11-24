{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , command :: Text.Text
  }

handleCommand :: RequestCommand -> HandlerM MCP.Server.Content
handleCommand MkRequestCommand {identifier, command} = do
  MkHandlerEnvironment {vms} <- getEnvironment
  inUse <- liftIO $ STM.atomically $ do
    pool <- STM.readTVar vms
    pure $
      case lookupVMLock pool identifier of
        Just (VMisAlreadyInUse _) -> True
        _ -> False
  if not inUse
    then pure $ MCP.Server.ContentText "ID is not in use."
    else MCP.Server.ContentText <$> sshCommand identifier command
