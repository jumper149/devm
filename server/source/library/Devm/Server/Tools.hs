module Devm.Server.Tools where

import Control.Monad.IO.Class
import Data.Kind
import Devm.Server.Environment
import Devm.Server.Tools.Command
import Devm.Server.Tools.Create
import Devm.Server.Tools.Destroy
import MCP.Server qualified

type Tool :: Type
data Tool
  = Create RequestCreate
  | Command RequestCommand
  | Destroy RequestDestroy
  deriving stock (Show)

handleTool :: MCP.Server.ClientContext -> Tool -> HandlerM MCP.Server.Content
handleTool _ctx tool = do
  liftIO $ putStrLn $ "Handling tool: " <> show tool
  case tool of
    Create request -> handleCreate request
    Command request -> handleCommand request
    Destroy request -> handleDestroy request
