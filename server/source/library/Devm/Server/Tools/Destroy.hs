{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Destroy where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as Text
import Data.Unique
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified
import System.IO qualified as IO
import System.Posix.Signals qualified as Signals
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
      _ <- lift $ waitForTermination identifier (runningProcess running)
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

terminationTimeout :: Int
terminationTimeout = 30_000_000

terminationPollInterval :: Int
terminationPollInterval = 10_000

waitForTermination :: VMID -> Process.ProcessHandle -> IO ()
waitForTermination identifier processHandle = go 0
 where
  go :: Int -> IO ()
  go elapsed
    | elapsed >= terminationTimeout = do
        IO.putStrLn $
          "VM "
            <> Text.unpack (displayVMID identifier)
            <> " did not exit within 30 seconds; sending SIGKILL."
        mPid <- Process.getPid processHandle
        case mPid of
          Nothing ->
            pure ()
          Just pid ->
            ignoreIOException $ Signals.signalProcess Signals.sigKILL pid
        _ <- Process.waitForProcess processHandle
        pure ()
    | otherwise = do
        exited <- Process.getProcessExitCode processHandle
        case exited of
          Just _ ->
            pure ()
          Nothing -> do
            Concurrent.threadDelay terminationPollInterval
            go (elapsed + terminationPollInterval)

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `Exception.catch` ignore
 where
  ignore :: Exception.IOException -> IO ()
  ignore _ = pure ()
