{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Create where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Unique
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
  lockStartupToken <- lift newUnique
  let lockStartup = VMisStarting lockStartupToken
  generator <- lift Random.newStdGen
  eResult <- mapHandlerT STM.atomically $ do
    pool <- lift $ STM.readTVar vms
    case findFreeVM generator pool of
      Nothing ->
        pure $ Left $ MCP.Server.ContentText "Error: No VM is available at this time."
      Just identifier ->
        getVMExecutable identifier >>= \case
          Nothing ->
            error "A VM, that was just found as free to use should not just disapper."
          Just executable -> do
            lift $ STM.writeTVar vms $ setVMLock identifier lockStartup pool
            pure $ Right (identifier, executable)
  case eResult of
    Left result -> pure result
    Right (identifier, executable) -> do
      let
        rollback :: HandlerT STM.STM ()
        rollback = do
          pool <- lift $ STM.readTVar vms
          lift $ STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
      running <-
        lift (VMProcess.startVM executable)
          `onException` mapHandlerT STM.atomically (continueStartup vms identifier lockStartupToken rollback)
      ready <- lift STM.newEmptyTMVarIO
      logVMs <- getLogVMs
      _ <- lift $ Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
      _ <- lift $ Concurrent.forkIO $ VMProcess.forwardOutput identifier sshDaemonStartedMarker ready logVMs running
      _ <- lift $ Concurrent.forkIO $ VMProcess.forwardError identifier logVMs running
      continueWithIO <- mapHandlerT STM.atomically $ continueStartup vms identifier lockStartupToken $ do
        started <- lift $ STM.takeTMVar ready
        if started
          then do
            lift $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
            pure $ pure $ MCP.Server.ContentText $ "Access token: " <> displayAccessToken (runningAccessToken running)
          else pure $ do
            stopFailedVM vms identifier lockStartupToken running
            pure $ MCP.Server.ContentText "Error: VM failed to start."
      lift continueWithIO

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

continueStartup :: STM.TVar VMPool -> VMID -> Unique -> HandlerT STM.STM a -> HandlerT STM.STM a
continueStartup vms identifier token continue = do
  pool <- lift $ STM.readTVar vms
  case lookupVMLock pool identifier of
    Just (VMisStarting current) ->
      if current == token
        then continue
        else error "Another startup owns this VM."
    Just VMisNotStarting ->
      error "Another actor changed the starting state."
    Nothing ->
      error "Another actor removed this VM from the pool."

stopFailedVM :: STM.TVar VMPool -> VMID -> Unique -> RunningVM -> IO ()
stopFailedVM vms identifier token running = do
  ignoreIOException $ Process.terminateProcess (runningProcess running)
  ignoreIOException $ IO.hClose (runningInput running)
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisStarting current) ->
        if current == token
          then STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
          else error "Another startup owns this VM."
      Just VMisNotStarting ->
        error "Another actor changed the starting state."
      Nothing ->
        error "Another actor removed this VM from the pool."

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `Exception.catch` ignore
 where
  ignore :: Exception.IOException -> IO ()
  ignore _ = pure ()
