{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Tools.Command where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.Environment
import Devm.Server.VM
import MCP.Server qualified

type RequestCommand :: Type
data RequestCommand = MkRequestCommand
  { identifier :: VMID
  , accessToken :: Text.Text
  , command :: Text.Text
  }
  deriving stock (Show)

handleCommand :: RequestCommand -> HandlerM MCP.Server.Content
handleCommand MkRequestCommand {identifier, accessToken, command} = do
  vms <- getVMState
  authorized <- liftIO $ STM.atomically $ do
    pool <- STM.readTVar vms
    pure $
      case lookupVMLock pool identifier of
        Just (VMisAlreadyInUse running)
          | MkVMAccessToken accessToken == runningAccessToken running -> True
        _ -> False
  if authorized
    then MCP.Server.ContentText <$> sshCommand identifier command
    else invalidAccess
