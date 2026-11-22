module Devm.Server.VM.Process where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Text qualified as Text
import Data.Unique qualified as Unique
import Devm.Server.VM
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

startVM :: VMExecutable -> IO RunningVM
startVM (MkVMExecutable path) = do
  token <- Unique.newUnique
  created <-
    Process.createProcess $
      (Process.proc (Text.unpack path) [])
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
  case created of
    (Just vmInput, Just vmOutput, Just vmError, vmProcess) ->
      pure MkRunningVM {vmToken = token, vmProcess, vmInput, vmOutput, vmError}
    _ ->
      Exception.throwIO $ IOError.userError "Could not create pipes for the VM executable."

monitorVM :: STM.TVar VMPool -> VMID -> RunningVM -> IO ()
monitorVM vms identifier running = do
  _ <- Process.waitForProcess (runningProcess running)
  IO.hClose (runningInput running)
  IO.hClose (runningOutput running)
  IO.hClose (runningError running)
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse current)
        | runningToken current == runningToken running ->
            STM.writeTVar vms $ unlockVM identifier pool
      _ ->
        pure ()
