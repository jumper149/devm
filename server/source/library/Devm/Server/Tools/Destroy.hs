{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Destroy where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as Text
import Data.Unique
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
  lockShutdownToken <- lift newUnique
  mRunning <- mapHandlerT STM.atomically $ do
    vms <- getVMState
    pool <- lift $ STM.readTVar vms
    case lookupRunningVMByAccessToken pool (MkVMAccessToken accessToken) of
      Nothing ->
        pure Nothing
      Just (identifier, running) -> do
        lift $ STM.writeTVar vms $ setVMLock identifier (VMisShuttingDown lockShutdownToken) pool
        pure $ Just (identifier, running)
  case mRunning of
    Nothing ->
      invalidAccess
    Just (identifier, running) -> do
      lift $ Process.terminateProcess (runningProcess running)
      lift $ IO.hClose (runningInput running)
      mapHandlerT STM.atomically $ do
        vms <- getVMState
        pool <- lift $ STM.readTVar vms
        case lookupVMLock pool identifier of
          Just (VMisShuttingDown token) ->
            if token == lockShutdownToken
              then do
                lift $ STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
                pure $ MCP.Server.ContentText "Destroyed VM."
              else error "Another actor tries to shutdown."
          Just VMisNotShuttingDown ->
            error "Another actor changed ignored the shutting-down state and modified it."
          Nothing ->
            error "Another actor already removed this VM from the pool."
