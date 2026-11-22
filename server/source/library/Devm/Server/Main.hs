{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Devm.Server.Main where

import Data.Map qualified as Map
import Devm.Server.CommandLine
import Devm.Server.Tools
import Devm.Server.VM
import MCP.Server qualified
import MCP.Server.Derive qualified
import Options.Applicative qualified as Opt

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
  MkCommandLine {cliVMExecutables = executables@(MkVMExecutables {unVMExecutables = executablesMap}), cliReadyMarker = readyMarker} <- Opt.execParser commandLineParserInfo
  vms <- initializeVMPool $ Map.keys executablesMap
  let environment = MkHandlerEnvironment {..}
  let tool = handleTool environment
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttp info handlers
