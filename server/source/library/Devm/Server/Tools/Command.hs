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
  , timeout :: Maybe Int
  }
  deriving stock (Show)

handleCommand :: RequestCommand -> HandlerT IO MCP.Server.Content
handleCommand MkRequestCommand {accessToken, command, timeout} = do
  vms <- getVMState
  defaultTimeout <- getCommandTimeoutDefault
  maxTimeout <- getCommandTimeoutMax
  authorized <- lift $ STM.atomically $ do
    pool <- STM.readTVar vms
    pure $ lookupRunningVMByAccessToken pool (MkVMAccessToken accessToken)
  case authorized of
    Nothing ->
      invalidAccess
    Just (identifier, _) ->
      case resolveCommandTimeout defaultTimeout maxTimeout timeout of
        Nothing ->
          pure $ MCP.Server.ContentText $ "Error: Command timeout is invalid. Maximum timeout is " <> Text.pack (show maxTimeout) <> " seconds."
        Just timeoutSeconds ->
          MCP.Server.ContentText <$> sshCommand identifier command timeoutSeconds

resolveCommandTimeout :: Int -> Int -> Maybe Int -> Maybe Int
resolveCommandTimeout defaultTimeout maxTimeout requested =
  case requested of
    Nothing ->
      Just defaultTimeout
    Just seconds ->
      if seconds <= maxTimeout
        then
          ( if seconds >= 1
              then Just seconds
              else Just 1
          )
        else Nothing
