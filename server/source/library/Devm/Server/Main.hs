{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Main where

import Control.Concurrent.STM qualified as STM
import Data.Kind
import Data.Text qualified as Text
import MCP.Server qualified
import MCP.Server.Derive qualified

type VMID :: Type
data VMID = MkVMID Int
  deriving stock (Eq, Ord)

displayVMID :: VMID -> Text.Text
displayVMID (MkVMID vmID) = Text.pack $ show vmID

type VMLock :: Type
data VMLock = VMisFreeToUse | VMisAlreadyInUse

type VMPool :: Type
newtype VMPool = MkVMPool {getVMState :: VMID -> VMLock}

initializeVMPool :: IO (STM.TVar VMPool)
initializeVMPool =
  STM.atomically $
    STM.newTVar $
      MkVMPool
        { getVMState = \_ -> VMisFreeToUse
        }

type HandlerEnvironment :: Type
newtype HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
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
handleTool MkHandlerEnvironment {vms} _ctx = \case
  Create MkRequestCreate {identifier} -> do
    STM.atomically $ do
      MkVMPool {getVMState} <- STM.readTVar vms
      case getVMState identifier of
        VMisFreeToUse -> do
          let newPool = MkVMPool $ \identifier' -> if identifier' == identifier then VMisAlreadyInUse else getVMState identifier'
          STM.writeTVar vms newPool
          pure $ MCP.Server.ContentText $ "You are now using ID: " <> displayVMID identifier
        VMisAlreadyInUse ->
          pure $ MCP.Server.ContentText "ID is already in use."
  Command MkRequestCommand {} -> pure $ MCP.Server.ContentText "Commanded"
  Destroy MkRequestDestroy {} -> pure $ MCP.Server.ContentText "Destroyed"

info :: MCP.Server.McpServerInfo
info =
  MCP.Server.McpServerInfo
    { MCP.Server.serverName = "devm"
    , MCP.Server.serverVersion = "0"
    , MCP.Server.serverInstructions = ""
    }

$(pure [])

main :: IO ()
main = do
  vms <- initializeVMPool
  let environment = MkHandlerEnvironment {..}
  let tool = handleTool environment
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttp info handlers
