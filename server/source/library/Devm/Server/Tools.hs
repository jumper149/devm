{-# LANGUAGE DuplicateRecordFields #-}

module Devm.Server.Tools (
  module Devm.Server.Tools.Command,
  module Devm.Server.Tools.Create,
  module Devm.Server.Tools.Destroy,
  Tool (..),
  handleTool,
) where

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

handleTool :: HandlerEnvironment -> MCP.Server.ClientContext -> Tool -> IO MCP.Server.Content
handleTool environment _ctx = \case
  Create request -> handleCreate environment request
  Command request -> handleCommand environment request
  Destroy request -> handleDestroy environment request
