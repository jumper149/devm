{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.VM where

import Control.Concurrent.STM qualified as STM
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Unique qualified as Unique
import System.IO qualified as IO
import System.Process qualified as Process

type VMID :: Type
data VMID = MkVMID Int
  deriving stock (Eq, Ord)

displayVMID :: VMID -> Text.Text
displayVMID (MkVMID vmID) = Text.pack $ show vmID

type RunningVM :: Type
data RunningVM = MkRunningVM
  { vmToken :: Unique.Unique
  , vmProcess :: Process.ProcessHandle
  , vmInput :: IO.Handle
  , vmOutput :: IO.Handle
  , vmError :: IO.Handle
  }

runningToken :: RunningVM -> Unique.Unique
runningToken MkRunningVM {vmToken} = vmToken

runningProcess :: RunningVM -> Process.ProcessHandle
runningProcess MkRunningVM {vmProcess} = vmProcess

runningInput :: RunningVM -> IO.Handle
runningInput MkRunningVM {vmInput} = vmInput

runningOutput :: RunningVM -> IO.Handle
runningOutput MkRunningVM {vmOutput} = vmOutput

runningError :: RunningVM -> IO.Handle
runningError MkRunningVM {vmError} = vmError

type VMLock :: Type
data VMLock = VMisFreeToUse | VMisStarting | VMisAlreadyInUse RunningVM | VMisShuttingDown

type VMExecutable :: Type
data VMExecutable = MkVMExecutable Text.Text

type VMExecutables :: Type
data VMExecutables = MkVMExecutables {unVMExecutables :: Map.Map VMID VMExecutable}

type VMPool :: Type
newtype VMPool = MkVMPool {unVMPool :: Map.Map VMID VMLock}

initializeVMPool :: [VMID] -> IO (STM.TVar VMPool)
initializeVMPool identifiers =
  STM.atomically $
    STM.newTVar $
      MkVMPool $
        Map.fromList $
          (,VMisFreeToUse) <$> identifiers

lookupVMLock :: VMPool -> VMID -> Maybe VMLock
lookupVMLock MkVMPool {unVMPool} identifier =
  Map.lookup identifier unVMPool

setVMLock :: VMID -> VMLock -> VMPool -> VMPool
setVMLock identifier lock (MkVMPool pool) =
  MkVMPool $ Map.insert identifier lock pool
