{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Create where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.IO.Class
import Data.Kind
import Devm.Server.Environment
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified

type RequestCreate :: Type
data RequestCreate = MkRequestCreate
  { identifier :: VMID
  }

handleCreate :: RequestCreate -> HandlerM MCP.Server.Content
handleCreate MkRequestCreate {identifier} = do
  vms <- getVMState
  getVMExecutable identifier >>= \case
    Nothing ->
      pure $ MCP.Server.ContentText "ID does not exist."
    Just executable -> do
      reserved <- liftIO $ STM.atomically $ do
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
            liftIO $
              VMProcess.startVM executable
                `Exception.onException` STM.atomically (STM.modifyTVar' vms (setVMLock identifier VMisFreeToUse))
          liftIO $ STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.forwardOutput identifier running
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.forwardError identifier running
          pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
