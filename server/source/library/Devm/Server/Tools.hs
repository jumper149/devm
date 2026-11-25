module Devm.Server.Tools where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class
import Data.Kind
import Devm.Server.Environment
import Devm.Server.Tools.Command
import Devm.Server.Tools.Create
import Devm.Server.Tools.Destroy
import MCP.Server qualified

type Tool :: Type
data Tool
  = Create
  | Command RequestCommand
  | Destroy RequestDestroy
  deriving stock (Show)

handleTool :: MCP.Server.ClientContext -> Tool -> HandlerM MCP.Server.Content
handleTool _ctx tool = do
  logRequests <- getLogRequests
  Monad.when logRequests $ liftIO $ putStrLn $ "Handling tool: " <> show tool
  response <- case tool of
    Create -> handleCreate
    Command request -> handleCommand request
    Destroy request -> handleDestroy request
  Monad.when logRequests $ liftIO $ putStrLn $ "Responding to tool (" <> show tool <> "): " <> take 100 (show response)
  pure response
