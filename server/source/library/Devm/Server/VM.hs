{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
  deriving stock (Eq, Ord, Show)

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
data VMLock
  = VMisFreeToUse
  | VMisStarting
  | VMisAlreadyInUse RunningVM
  | VMisShuttingDown

{-# COMPLETE VMisFreeToUse, VMisNotFreeToUse #-}
pattern VMisNotFreeToUse :: VMLock
pattern VMisNotFreeToUse <-
  ( \case
      VMisFreeToUse -> Nothing
      x -> Just x ->
      Just _
    )

{-# COMPLETE VMisStarting, VMisNotStarting #-}
pattern VMisNotStarting :: VMLock
pattern VMisNotStarting <-
  ( \case
      VMisStarting -> Nothing
      x -> Just x ->
      Just _
    )

{-# COMPLETE VMisAlreadyInUse, VMisNotAlreadyInUse #-}
pattern VMisNotAlreadyInUse :: VMLock
pattern VMisNotAlreadyInUse <-
  ( \case
      VMisAlreadyInUse _ -> Nothing
      x -> Just x ->
      Just _
    )

{-# COMPLETE VMisShuttingDown, VMisNotShuttingDown #-}
pattern VMisNotShuttingDown :: VMLock
pattern VMisNotShuttingDown <-
  ( \case
      VMisShuttingDown -> Nothing
      x -> Just x ->
      Just _
    )

type VMExecutable :: Type
data VMExecutable = MkVMExecutable Text.Text

type VMExecutables :: Type
data VMExecutables = MkVMExecutables {unVMExecutables :: Map.Map VMID VMExecutable}

type VMPool :: Type
data VMPool = MkVMPool
  { unVMPool :: Map.Map VMID VMLock
  , unVMAccessTokenIndex :: Map.Map VMAccessToken VMID
  }

initializeVMPool :: [VMID] -> IO (STM.TVar VMPool)
initializeVMPool identifiers =
  STM.atomically $
    STM.newTVar $
      MkVMPool
        { unVMPool = Map.fromList $ (,VMisFreeToUse) <$> identifiers
        , unVMAccessTokenIndex = Map.empty
        }

lookupVMLock :: VMPool -> VMID -> Maybe VMLock
lookupVMLock MkVMPool {unVMPool} identifier =
  Map.lookup identifier unVMPool

setVMLock :: VMID -> VMLock -> VMPool -> VMPool
setVMLock identifier lock MkVMPool {unVMPool, unVMAccessTokenIndex} =
  MkVMPool
    { unVMPool = Map.insert identifier lock unVMPool
    , unVMAccessTokenIndex =
        updateAccessTokenIndex identifier (Map.lookup identifier unVMPool) lock unVMAccessTokenIndex
    }

updateAccessTokenIndex ::
  VMID ->
  Maybe VMLock ->
  VMLock ->
  Map.Map VMAccessToken VMID ->
  Map.Map VMAccessToken VMID
updateAccessTokenIndex identifier previousLock lock index =
  let withoutPrevious =
        case previousLock of
          Just (VMisAlreadyInUse running) ->
            Map.update (remove identifier) (runningAccessToken running) index
          Just VMisNotAlreadyInUse ->
            index
          Nothing ->
            index
   in case lock of
        VMisAlreadyInUse running ->
          Map.insert (runningAccessToken running) identifier withoutPrevious
        VMisNotAlreadyInUse ->
          withoutPrevious
 where
  remove :: VMID -> VMID -> Maybe VMID
  remove expected current
    | expected == current = Nothing
    | otherwise = Just current

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

findFreeVM :: VMPool -> IO (Maybe VMID)
findFreeVM pool =
  case freeVMs pool of
    [] ->
      pure Nothing
    identifiers -> do
      index <- Random.randomRIO (0, length identifiers - 1)
      pure $ Just $ identifiers !! index

reserveRandomVM :: STM.TVar VMPool -> IO (Maybe VMID)
reserveRandomVM vms = do
  pool <- STM.atomically $ STM.readTVar vms
  mIdentifier <- findFreeVM pool
  case mIdentifier of
    Nothing ->
      pure Nothing
    Just identifier -> do
      reserved <- STM.atomically $ do
        current <- STM.readTVar vms
        case lookupVMLock current identifier of
          Just VMisFreeToUse -> do
            STM.writeTVar vms $ setVMLock identifier VMisStarting current
            pure True
          Just VMisNotFreeToUse ->
            pure False
          Nothing ->
            pure False
      if reserved
        then pure $ Just identifier
        else reserveRandomVM vms

lookupVMIDByAccessToken :: VMPool -> VMAccessToken -> Maybe VMID
lookupVMIDByAccessToken MkVMPool {unVMAccessTokenIndex} accessToken =
  Map.lookup accessToken unVMAccessTokenIndex

lookupRunningVMByAccessToken :: VMPool -> VMAccessToken -> Maybe (VMID, RunningVM)
lookupRunningVMByAccessToken pool accessToken = do
  identifier <- lookupVMIDByAccessToken pool accessToken
  running <-
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse current)
        | runningAccessToken current == accessToken -> Just current
      Just (VMisAlreadyInUse _) -> Nothing
      Just VMisNotAlreadyInUse -> Nothing
      Nothing -> Nothing
  pure (identifier, running)
