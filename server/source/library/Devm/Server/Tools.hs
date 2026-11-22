{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Devm.Server.VM
import Devm.Server.VM.Process qualified as VMProcess
import MCP.Server qualified
import System.IO qualified as IO
import System.Process qualified as Process

type HandlerEnvironment :: Type
data HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
  , executables :: VMExecutables
  }

type RequestCreate :: Type
data RequestCreate = MkRequestCreate
  { identifier :: VMID
  }

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , command :: Text.Text
  }

type RequestDestroy :: Type
data RequestDestroy = MkRequestDestroy
  { identifier :: VMID
  , destroy :: Text.Text
  }

type Tool :: Type
data Tool
  = Create RequestCreate
  | Command RequestCommand
  | Destroy RequestDestroy

handleTool :: HandlerEnvironment -> MCP.Server.ClientContext -> Tool -> IO MCP.Server.Content
handleTool MkHandlerEnvironment {vms, executables = MkVMExecutables {unVMExecutables = executableMap}} _ctx = \case
  Create MkRequestCreate {identifier} ->
    case Map.lookup identifier executableMap of
      Nothing ->
        pure $ MCP.Server.ContentText "ID does not exist."
      Just executable -> do
        reserved <- STM.atomically $ do
          pool <- STM.readTVar vms
          case lookupVMLock pool identifier of
            Just VMisFreeToUse -> do
              STM.writeTVar vms $ setVMLock identifier VMisStarting pool
              pure True
            _ ->
              pure False
        if not reserved
          then pure $ MCP.Server.ContentText "ID is already in use."
          else do
            running <-
              VMProcess.startVM executable
                `Exception.onException` STM.atomically (STM.modifyTVar' vms (unlockVM identifier))
            STM.atomically $ STM.modifyTVar' vms (setVMLock identifier (VMisAlreadyInUse running))
            _ <- Concurrent.forkIO $ VMProcess.monitorVM vms identifier running
            pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
  Command MkRequestCommand {identifier, command} -> do
    mRunning <- STM.atomically $ do
      pool <- STM.readTVar vms
      pure $
        case lookupVMLock pool identifier of
          Just (VMisAlreadyInUse running) -> Just running
          _ -> Nothing
    case mRunning of
      Nothing ->
        pure $ MCP.Server.ContentText "ID is not in use."
      Just running -> do
        IO.hPutStrLn (runningInput running) (Text.unpack command)
        IO.hFlush (runningInput running)
        response <- IO.hGetLine (runningOutput running)
        pure $ MCP.Server.ContentText $ Text.pack response
  Destroy MkRequestDestroy {identifier} -> do
    mRunning <- STM.atomically $ do
      pool <- STM.readTVar vms
      case lookupVMLock pool identifier of
        Just (VMisAlreadyInUse running) -> do
          STM.writeTVar vms $ unlockVM identifier pool
          pure $ Just running
        _ ->
          pure Nothing
    case mRunning of
      Nothing ->
        pure $ MCP.Server.ContentText "ID is not in use."
      Just running -> do
        Process.terminateProcess (runningProcess running)
        IO.hClose (runningInput running)
        IO.hClose (runningOutput running)
        IO.hClose (runningError running)
        pure $ MCP.Server.ContentText "Destroyed"
