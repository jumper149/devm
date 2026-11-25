{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.VM where

import Control.Concurrent.STM qualified as STM
import Data.Bits ((.&.), (.|.))
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Data.Unique qualified as Unique
import System.IO qualified as IO
import System.Process qualified as Process
import System.Random qualified as Random

type VMID :: Type
data VMID = MkVMID Int
  deriving stock (Eq, Ord, Show)

displayVMID :: VMID -> Text.Text
displayVMID (MkVMID vmID) = Text.pack $ show vmID

type VMAccessToken :: Type
newtype VMAccessToken = MkVMAccessToken {unVMAccessToken :: Text.Text}
  deriving stock (Eq, Show)

displayAccessToken :: VMAccessToken -> Text.Text
displayAccessToken MkVMAccessToken {unVMAccessToken} = unVMAccessToken

newVMAccessToken :: IO VMAccessToken
newVMAccessToken = do
  word1 <- Random.randomIO
  word2 <- Random.randomIO
  word3 <- Random.randomIO
  word4 <- Random.randomIO
  let uuid =
        UUID.fromWords
          word1
          ((word2 .&. 0xffff0fff) .|. 0x00004000)
          ((word3 .&. 0x3fffffff) .|. 0x80000000)
          word4
  pure $ MkVMAccessToken $ UUID.toText uuid

type RunningVM :: Type
data RunningVM = MkRunningVM
  { vmLifetimeToken :: Unique.Unique
  , vmAccessToken :: VMAccessToken
  , vmProcess :: Process.ProcessHandle
  , vmInput :: IO.Handle
  , vmOutput :: IO.Handle
  , vmError :: IO.Handle
  }

runningLifetimeToken :: RunningVM -> Unique.Unique
runningLifetimeToken MkRunningVM {vmLifetimeToken} = vmLifetimeToken

runningAccessToken :: RunningVM -> VMAccessToken
runningAccessToken MkRunningVM {vmAccessToken} = vmAccessToken

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
