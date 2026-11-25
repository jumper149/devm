{-# LANGUAGE TemplateHaskell #-}

module Devm.Server.Main where

import Data.Map qualified as Map
import Devm.Server.CLI
import Devm.Server.Environment
import Devm.Server.Tools
import Devm.Server.VM
import MCP.Server qualified
import MCP.Server.Derive qualified
import Options.Applicative qualified as Opt
import System.IO qualified as IO

info :: MCP.Server.McpServerInfo
info =
  MCP.Server.McpServerInfo
    { MCP.Server.serverName = "devm"
    , MCP.Server.serverVersion = "0"
    , MCP.Server.serverInstructions = ""
    }

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  MkCommandLine {cliVMExecutables = executables@(MkVMExecutables {unVMExecutables = executablesMap}), cliReadyMarker = readyMarker, cliSSHKey = sshKeyFile} <- Opt.execParser commandLineParserInfo
  vms <- initializeVMPool $ Map.keys executablesMap
  let environment = MkHandlerEnvironment {..}
  let tool ctx request = runHandlerM environment (handleTool ctx request)
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttp info handlers
