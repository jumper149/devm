{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Create where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.Trans.Class
import Data.Map qualified as Map
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified
import System.IO qualified as IO
import System.Process qualified as Process
import System.Random qualified as Random

handleCreate :: HandlerT IO MCP.Server.Content
handleCreate = do
  vms <- getVMState
  generator <- lift Random.newStdGen
  reserved <- lift $ STM.atomically $ reserveVM generator vms
  case reserved of
    Nothing ->
      pure $ MCP.Server.ContentText "No VM is available."
    Just identifier ->
      getVMExecutable identifier >>= \case
        Nothing -> do
          lift $ STM.atomically $ STM.modifyTVar' vms (setVMLock identifier VMisFreeToUse)
          pure $ MCP.Server.ContentText "VM does not exist."
        Just executable -> do
          running <-
            lift $
              VMProcess.startVM executable
                `Exception.onException` STM.atomically (STM.modifyTVar' vms (setVMLock identifier VMisFreeToUse))
          lift $ STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
          ready <- lift STM.newEmptyTMVarIO
          logVMs <- getLogVMs
          _ <- lift $ Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
          _ <- lift $ Concurrent.forkIO $ VMProcess.forwardOutput identifier sshDaemonStartedMarker ready logVMs running
          _ <- lift $ Concurrent.forkIO $ VMProcess.forwardError identifier logVMs running
          started <- lift $ STM.atomically $ STM.takeTMVar ready
          if started
            then
              pure $
                MCP.Server.ContentText $
                  "Access token: " <> displayAccessToken (runningAccessToken running)
            else do
              lift $ stopFailedVM vms identifier running
              pure $ MCP.Server.ContentText "VM did not report 'Started SSH Daemon' on stdout."

sshDaemonStartedMarker :: Text.Text
sshDaemonStartedMarker = "Started SSH Daemon"

freeVMs :: VMPool -> [VMID]
freeVMs MkVMPool {unVMPool} =
  Map.foldrWithKey addFreeVM [] unVMPool
 where
  addFreeVM :: VMID -> VMLock -> [VMID] -> [VMID]
  addFreeVM identifier lock identifiers =
    case lock of
      VMisFreeToUse ->
        identifier : identifiers
      VMisNotFreeToUse ->
        identifiers

findFreeVM :: Random.StdGen -> VMPool -> Maybe VMID
findFreeVM generator pool =
  case freeVMs pool of
    [] ->
      Nothing
    identifiers ->
      let (index, _) = Random.uniformR (0, length identifiers - 1) generator
       in Just $ identifiers !! index

reserveVM :: Random.StdGen -> STM.TVar VMPool -> STM.STM (Maybe VMID)
reserveVM generator vms = do
  pool <- STM.readTVar vms
  case findFreeVM generator pool of
    Nothing ->
      pure Nothing
    Just identifier -> do
      STM.writeTVar vms $ setVMLock identifier VMisStarting pool
      pure $ Just identifier

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
      Just (VMisAlreadyInUse _) ->
        pure ()
      Just VMisNotAlreadyInUse ->
        pure ()
      Nothing ->
        pure ()

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `Exception.catch` ignore
 where
  ignore :: Exception.IOException -> IO ()
  ignore _ = pure ()
