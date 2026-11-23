{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Create where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Kind
import Data.Map qualified as Map
import Devm.Server.Environment
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified

type RequestCreate :: Type
data RequestCreate = MkRequestCreate
  { identifier :: VMID
  }

handleCreate :: HandlerEnvironment -> RequestCreate -> IO MCP.Server.Content
handleCreate MkHandlerEnvironment {vms, executables = MkVMExecutables {unVMExecutables = executableMap}, readyMarker} MkRequestCreate {identifier} =
  case Map.lookup identifier executableMap of
    Nothing ->
      pure $ MCP.Server.ContentText "ID does not exist."
    Just executable -> do
      reserved <- STM.atomically $ do
        pool <- STM.readTVar vms
        case lookupVMLock pool identifier of
          Just VMisFreeToUse -> do
            STM.writeTVar vms $ setVMLock identifier VMisStarting pool
            pure True
          _ ->
            pure False
      if not reserved
        then pure $ MCP.Server.ContentText "ID is already in use."
        else do
          running <-
            VMProcess.startVM executable
              `Exception.onException` STM.atomically (STM.modifyTVar' vms (unlockVM identifier))
          STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
          _ <- Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
          _ <- Concurrent.forkIO $ VMProcess.forwardStartup identifier readyMarker running
          _ <- Concurrent.forkIO $ VMProcess.discardOutput (runningError running)
          pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
