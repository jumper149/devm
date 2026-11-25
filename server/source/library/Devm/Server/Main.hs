{-# LANGUAGE TemplateHaskell #-}

module Devm.Server.Main where

import Data.Map qualified as Map
import Devm.Server.CLI
import Devm.Server.Environment.Internal qualified as Internal
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
  MkCommandLine
    { cliVMExecutables = executables@(MkVMExecutables {unVMExecutables = executablesMap})
    , cliSSHKey = sshKeyFile
    , cliHttpHost
    , cliHttpPort
    } <-
    Opt.execParser commandLineParserInfo
  vms <- initializeVMPool $ Map.keys executablesMap
  let httpConfig = MCP.Server.defaultHttpConfig {MCP.Server.httpHost = cliHttpHost, MCP.Server.httpPort = cliHttpPort}
  let environment = Internal.MkHandlerEnvironment {..}
  let tool ctx request = Internal.runHandlerM environment (handleTool ctx request)
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttpWithConfig httpConfig info handlers
