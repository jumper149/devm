{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Destroy where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified
import System.IO qualified as IO
import System.Process qualified as Process

type RequestDestroy :: Type
data RequestDestroy = MkRequestDestroy
  { identifier :: VMID
  , accessToken :: Text.Text
  , destroy :: Text.Text
  }
  deriving stock (Show)

handleDestroy :: RequestDestroy -> HandlerM MCP.Server.Content
handleDestroy MkRequestDestroy {identifier, accessToken} = do
  vms <- getVMState
  mRunning <- liftIO $ STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse running)
        | MkVMAccessToken accessToken == runningAccessToken running -> do
            STM.writeTVar vms $ setVMLock identifier VMisShuttingDown pool
            pure $ Just running
      _ -> pure Nothing
  case mRunning of
    Nothing ->
      invalidAccess
    Just running -> do
      liftIO $ Process.terminateProcess (runningProcess running)
      liftIO $ IO.hClose (runningInput running)
      liftIO $ STM.atomically $ do
        pool <- STM.readTVar vms
        case lookupVMLock pool identifier of
          Just VMisShuttingDown -> do
            STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
          _ -> undefined
      pure $ MCP.Server.ContentText "Destroyed"
