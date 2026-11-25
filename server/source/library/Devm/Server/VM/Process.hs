module Devm.Server.VM.Process where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Text qualified as Text
import Data.Unique qualified as Unique
import Devm.Server.VM
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Process qualified as Process

startVM :: VMExecutable -> IO RunningVM
startVM (MkVMExecutable path) = do
  lifetimeToken <- Unique.newUnique
  accessToken <- newVMAccessToken
  created <-
    Process.createProcess $
      (Process.proc (Text.unpack path) [])
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
  case created of
    (Just vmInput, Just vmOutput, Just vmError, vmProcess) ->
      pure MkRunningVM {vmLifetimeToken = lifetimeToken, vmAccessToken = accessToken, vmProcess, vmInput, vmOutput, vmError}
    _ ->
      Exception.throwIO $ IOError.userError "Could not create pipes for the VM executable."

monitorVM :: STM.TVar VMPool -> VMID -> RunningVM -> IO ()
monitorVM vms identifier running = do
  _ <- Process.waitForProcess (runningProcess running)
  IO.hClose (runningInput running)
  STM.atomically $ do
    pool <- STM.readTVar vms
    case lookupVMLock pool identifier of
      Just (VMisAlreadyInUse current)
        | runningLifetimeToken current == runningLifetimeToken running ->
            STM.writeTVar vms $ setVMLock identifier VMisFreeToUse pool
      _ ->
        pure ()

forwardOutput :: VMID -> Text.Text -> STM.TMVar Bool -> Bool -> RunningVM -> IO ()
forwardOutput identifier marker ready logVMs running =
  go `Exception.catch` onError
 where
  prefix :: String
  prefix = "[vm " <> Text.unpack (displayVMID identifier) <> "] "

  handle :: IO.Handle
  handle = runningOutput running

  finish :: Bool -> IO ()
  finish started = do
    _ <- STM.atomically $ STM.tryPutTMVar ready started
    IO.hClose handle `Exception.catch` ignoreIOError

  onError :: Exception.IOException -> IO ()
  onError _ = finish False

  go :: IO ()
  go = do
    eof <- IO.hIsEOF handle
    if eof
      then finish False
      else do
        line <- IO.hGetLine handle
        Monad.when logVMs $ IO.putStrLn $ prefix <> line
        Monad.when (marker `Text.isInfixOf` stripAnsi (Text.pack line)) $ do
          _ <- STM.atomically $ STM.tryPutTMVar ready True
          pure ()
        go

stripAnsi :: Text.Text -> Text.Text
stripAnsi = Text.pack . go . Text.unpack
 where
  go :: String -> String
  go [] = []
  go ('\ESC' : '[' : rest) = go $ dropCsi rest
  go (char : rest) = char : go rest

  dropCsi :: String -> String
  dropCsi [] = []
  dropCsi (char : rest)
    | char >= '\x40' && char <= '\x7E' = rest
    | otherwise = dropCsi rest

forwardError :: VMID -> Bool -> RunningVM -> IO ()
forwardError identifier logVMs running =
  forwardLines prefix logVMs (runningError running)
 where
  prefix :: String
  prefix = "[vm " <> Text.unpack (displayVMID identifier) <> ":err] "

forwardLines :: String -> Bool -> IO.Handle -> IO ()
forwardLines prefix logVMs handle =
  go `Exception.catch` ignoreIOError
 where
  go :: IO ()
  go = do
    eof <- IO.hIsEOF handle
    if eof
      then IO.hClose handle
      else do
        line <- IO.hGetLine handle
        Monad.when logVMs $ IO.putStrLn $ prefix <> line
        go

ignoreIOError :: Exception.IOException -> IO ()
ignoreIOError _ = pure ()
