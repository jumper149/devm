{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.VM where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.UUID.Types qualified as UUID
import Data.Unique qualified as Unique
import Data.Word (Word8)
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

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
  bytes <- readRandomBytes 16
  let versioned =
        case bytes of
          byte0 : byte1 : byte2 : byte3 : byte4 : byte5 : byte6 : byte7 : rest ->
            [ byte0
            , byte1
            , byte2
            , byte3
            , byte4
            , byte5
            , (byte6 .&. 0x0f) .|. 0x40
            , (byte7 .&. 0x3f) .|. 0x80
            ]
              <> rest
          _ ->
            bytes
  case UUID.fromByteString (BL.fromStrict $ BS.pack versioned) of
    Nothing ->
      Exception.throwIO $ IOError.userError "Could not create an access token."
    Just uuid ->
      pure $ MkVMAccessToken $ UUID.toText uuid
 where
  readRandomBytes :: Int -> IO [Word8]
  readRandomBytes count =
    IO.withBinaryFile "/dev/urandom" IO.ReadMode $ \handle -> go handle count []
   where
    go :: IO.Handle -> Int -> [Word8] -> IO [Word8]
    go handle remaining accumulated
      | remaining <= 0 = pure accumulated
      | otherwise = do
          chunk <- BS.hGetSome handle remaining
          if BS.null chunk
            then Exception.throwIO $ IOError.userError "Could not read random bytes for the access token."
            else go handle (remaining - BS.length chunk) (accumulated <> BS.unpack chunk)

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
