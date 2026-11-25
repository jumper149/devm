{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Create where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.IO.Class
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified
import System.IO qualified as IO
import System.Process qualified as Process

sshDaemonStartedMarker :: Text.Text
sshDaemonStartedMarker = "Started SSH Daemon"

handleCreate :: HandlerM MCP.Server.Content
handleCreate = do
  vms <- getVMState
  reserved <- liftIO $ STM.atomically $ do
    pool <- STM.readTVar vms
    case findFreeVM pool of
      Nothing ->
        pure Nothing
      Just identifier -> do
        STM.writeTVar vms $ setVMLock identifier VMisStarting pool
        pure $ Just identifier
  case reserved of
    Nothing ->
      pure $ MCP.Server.ContentText "No VM is available."
    Just identifier ->
      getVMExecutable identifier >>= \case
        Nothing -> do
          liftIO $ STM.atomically $ STM.modifyTVar' vms (setVMLock identifier VMisFreeToUse)
          pure $ MCP.Server.ContentText "VM does not exist."
        Just executable -> do
          running <-
            liftIO $
              VMProcess.startVM executable
                `Exception.onException` STM.atomically (STM.modifyTVar' vms (setVMLock identifier VMisFreeToUse))
          liftIO $ STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
          ready <- liftIO STM.newEmptyTMVarIO
          logVMs <- getLogVMs
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.forwardOutput identifier sshDaemonStartedMarker ready logVMs running
          _ <- liftIO $ Concurrent.forkIO $ VMProcess.forwardError identifier logVMs running
          started <- liftIO $ STM.atomically $ STM.takeTMVar ready
          if started
            then
              pure $
                MCP.Server.ContentText $
                  "Access token: " <> displayAccessToken (runningAccessToken running)
            else do
              liftIO $ stopFailedVM vms identifier running
              pure $ MCP.Server.ContentText "VM did not report 'Started SSH Daemon' on stdout."

stopFailedVM :: STM.TVar VMPool -> VMID -> RunningVM -> IO ()
stopFailedVM vms identifier running = do
  ignoreIOException $ Process.terminateProcess (runningProcess running)
  ignoreIOException $ IO.hClose (runningInput running)
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse current)
        | runningLifetimeToken current == runningLifetimeToken running ->
            STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
      _ ->
        pure ()

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `Exception.catch` ignore
 where
  ignore :: Exception.IOException -> IO ()
  ignore _ = pure ()
