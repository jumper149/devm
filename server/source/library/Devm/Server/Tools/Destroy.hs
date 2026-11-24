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
  , destroy :: Text.Text
  }

handleDestroy :: RequestDestroy -> HandlerM MCP.Server.Content
handleDestroy MkRequestDestroy {identifier} = do
  MkHandlerEnvironment {vms} <- getEnvironment
  mRunning <- liftIO $ STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse running) -> do
        STM.writeTVar vms $ setVMLock identifier VMisShuttingDown pool
        pure $ Just running
      _ -> pure Nothing
  case mRunning of
    Nothing ->
      pure $ MCP.Server.ContentText "ID is not in use."
    Just running -> do
      liftIO $ Process.terminateProcess (runningProcess running)
      liftIO $ IO.hClose (runningInput running)
      liftIO $ IO.hClose (runningOutput running)
      liftIO $ IO.hClose (runningError running)
      liftIO $ STM.atomically $ do
        pool <- STM.readTVar vms
        case lookupVMLock pool identifier of
          Just VMisShuttingDown -> do
            STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
          _ -> undefined
      pure $ MCP.Server.ContentText "Destroyed"
