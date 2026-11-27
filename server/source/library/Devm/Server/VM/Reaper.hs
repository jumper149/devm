module Devm.Server.VM.Reaper where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Map qualified as Map
import Data.Unique qualified as Unique
import Data.Word qualified as Word
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import GHC.Clock qualified as Clock
import System.IO qualified as IO

startStaleVMReaper :: STM.TVar VMPool -> Int -> IO ()
startStaleVMReaper vms staleTimeoutSeconds = do
  _ <- Concurrent.forkIO $ reapStaleVMs vms staleTimeoutSeconds
  pure ()

staleVMReaperInterval :: Int
staleVMReaperInterval = 1_000_000

reapStaleVMs :: STM.TVar VMPool -> Int -> IO ()
reapStaleVMs vms staleTimeoutSeconds =
  Monad.forever $ do
    Concurrent.threadDelay staleVMReaperInterval
    now <- Clock.getMonotonicTimeNSec
    Exception.catch (reapStaleVMsOnce vms staleTimeoutNanoseconds now) reportReaperError
 where
  staleTimeoutNanoseconds :: Word.Word64
  staleTimeoutNanoseconds = fromIntegral staleTimeoutSeconds * 1_000_000_000

  reportReaperError :: Exception.SomeException -> IO ()
  reportReaperError err = IO.putStrLn $ "Stale VM reaper error: " <> show err

reapStaleVMsOnce :: STM.TVar VMPool -> Word.Word64 -> Word.Word64 -> IO ()
reapStaleVMsOnce vms timeoutNanoseconds now = do
  pool <- STM.readTVarIO vms
  Monad.forM_ (staleRunningVMs timeoutNanoseconds now pool) $ \(identifier, _running) -> do
    shutdownToken <- Unique.newUnique
    mClaimed <- STM.atomically $ claimStaleVM timeoutNanoseconds now shutdownToken vms identifier
    case mClaimed of
      Nothing ->
        pure ()
      Just (claimedIdentifier, claimedRunning) ->
        Exception.finally
          (VMProcess.terminateRunningVM claimedIdentifier claimedRunning)
          (finishStaleVMShutdown vms claimedIdentifier shutdownToken)

staleRunningVMs :: Word.Word64 -> Word.Word64 -> VMPool -> [(VMID, RunningVM)]
staleRunningVMs timeoutNanoseconds now MkVMPool {unVMPool} =
  Map.foldrWithKey addStale [] unVMPool
 where
  addStale :: VMID -> VMLock -> [(VMID, RunningVM)] -> [(VMID, RunningVM)]
  addStale identifier lock stale =
    case lock of
      VMisFreeToUse ->
        stale
      VMisStarting _ ->
        stale
      VMisShuttingDown _ ->
        stale
      VMisAlreadyInUse running ->
        if isStaleVM timeoutNanoseconds now running
          then (identifier, running) : stale
          else stale

isStaleVM :: Word.Word64 -> Word.Word64 -> RunningVM -> Bool
isStaleVM timeoutNanoseconds now running =
  now >= lastTouched && now - lastTouched >= timeoutNanoseconds
 where
  lastTouched :: Word.Word64
  lastTouched = vmLastTouched running

claimStaleVM ::
  Word.Word64 ->
  Word.Word64 ->
  Unique.Unique ->
  STM.TVar VMPool ->
  VMID ->
  STM.STM (Maybe (VMID, RunningVM))
claimStaleVM timeoutNanoseconds now shutdownToken vms identifier = do
  pool <- STM.readTVar vms
  case lookupVMLock pool identifier of
    Just (VMisAlreadyInUse running) ->
      if isStaleVM timeoutNanoseconds now running
        then do
          STM.writeTVar vms $ setVMLock identifier (VMisShuttingDown shutdownToken) pool
          pure $ Just (identifier, running)
        else pure Nothing
    Just VMisFreeToUse ->
      pure Nothing
    Just (VMisStarting _) ->
      pure Nothing
    Just (VMisShuttingDown _) ->
      pure Nothing
    Nothing ->
      pure Nothing

finishStaleVMShutdown :: STM.TVar VMPool -> VMID -> Unique.Unique -> IO ()
finishStaleVMShutdown vms identifier shutdownToken =
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisShuttingDown current) ->
        if current == shutdownToken
          then STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
          else error "Another actor owns this VM shutdown."
      Just VMisFreeToUse ->
        error "Another actor already freed this VM."
      Just (VMisStarting _) ->
        error "Another actor started this VM during shutdown."
      Just (VMisAlreadyInUse _) ->
        error "Another actor reused this VM during shutdown."
      Nothing ->
        error "Another actor removed this VM from the pool."
