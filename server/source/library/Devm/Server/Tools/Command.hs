{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { accessToken :: Text.Text
  , command :: Text.Text
  }
  deriving stock (Show)

handleCommand :: RequestCommand -> HandlerT IO MCP.Server.Content
handleCommand MkRequestCommand {accessToken, command} = do
  vms <- getVMState
  authorized <- lift $ STM.atomically $ do
    pool <- STM.readTVar vms
    pure $ lookupRunningVMByAccessToken pool (MkVMAccessToken accessToken)
  case authorized of
    Just (identifier, _) ->
      MCP.Server.ContentText <$> sshCommand identifier command
    Nothing ->
      invalidAccess
