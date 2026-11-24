{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

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
handleCommand MkRequestCommand {identifier, command} =
  getRunningVM identifier >>= \case
    Nothing -> pure $ MCP.Server.ContentText "ID is not in use."
    Just _ -> MCP.Server.ContentText <$> sshCommand identifier command
