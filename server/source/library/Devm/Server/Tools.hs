module Devm.Server.Tools where

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

handleTool :: MCP.Server.ClientContext -> Tool -> HandlerM MCP.Server.Content
handleTool _ctx = \case
  Create request -> handleCreate request
  Command request -> handleCommand request
  Destroy request -> handleDestroy request
