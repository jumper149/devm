{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Destroy where

import Control.Concurrent.STM qualified as STM
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

handleDestroy :: HandlerEnvironment -> RequestDestroy -> IO MCP.Server.Content
handleDestroy MkHandlerEnvironment {vms} MkRequestDestroy {identifier} = do
  mRunning <- STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse running) -> do
        STM.writeTVar vms $ unlockVM identifier pool
        pure $ Just running
      _ ->
        pure Nothing
  case mRunning of
    Nothing ->
      pure $ MCP.Server.ContentText "ID is not in use."
    Just running -> do
      Process.terminateProcess (runningProcess running)
      IO.hClose (runningInput running)
      IO.hClose (runningOutput running)
      IO.hClose (runningError running)
      pure $ MCP.Server.ContentText "Destroyed"
