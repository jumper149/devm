{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import GHC.Clock qualified as Clock
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
  now <- lift Clock.getMonotonicTimeNSec
  result <- lift $ STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupRunningVMByAccessToken pool (MkVMAccessToken accessToken) of
      Nothing ->
        pure Nothing
      Just (identifier, running) ->
        case resolveCommandTimeout defaultTimeout maxTimeout timeout of
          Nothing ->
            pure $
              Just $
                Left $
                  "Error: Command timeout is invalid. Maximum timeout is "
                    <> Text.pack (show maxTimeout)
                    <> " seconds."
          Just timeoutSeconds -> do
            let touched = setRunningLastTouched now running
            STM.writeTVar vms $ setVMLock identifier (VMisAlreadyInUse touched) pool
            pure $ Just $ Right (identifier, touched, timeoutSeconds)
  case result of
    Nothing ->
      invalidAccess
    Just (Left message) ->
      pure $ MCP.Server.ContentText message
    Just (Right (identifier, _touchedRunning, timeoutSeconds)) ->
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
