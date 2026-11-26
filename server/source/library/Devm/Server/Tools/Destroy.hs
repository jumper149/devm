{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Destroy where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified
import System.IO qualified as IO
import System.Process qualified as Process

type RequestDestroy :: Type
newtype RequestDestroy = MkRequestDestroy
  { accessToken :: Text.Text
  }
  deriving stock (Show)

handleDestroy :: RequestDestroy -> HandlerT IO MCP.Server.Content
handleDestroy MkRequestDestroy {accessToken} = do
  vms <- getVMState
  mRunning <- lift $ STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupRunningVMByAccessToken pool (MkVMAccessToken accessToken) of
      Just (identifier, running) -> do
        STM.writeTVar vms $ setVMLock identifier VMisShuttingDown pool
        pure $ Just (identifier, running)
      Nothing ->
        pure Nothing
  case mRunning of
    Nothing ->
      invalidAccess
    Just (identifier, running) -> do
      lift $ Process.terminateProcess (runningProcess running)
      lift $ IO.hClose (runningInput running)
      lift $ STM.atomically $ do
        pool <- STM.readTVar vms
        case lookupVMLock pool identifier of
          Just VMisShuttingDown ->
            STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
          Just VMisNotShuttingDown ->
            pure ()
          Nothing ->
            pure ()
      pure $ MCP.Server.ContentText "Destroyed"
