{-# LANGUAGE TemplateHaskell #-}

module Devm.Server.Main where

import Control.Monad qualified as Monad
import Data.Map qualified as Map
import Devm.Server.CLI
import Devm.Server.Environment.Internal qualified as Internal
import Devm.Server.Tools
import Devm.Server.VM
import Devm.Server.VM.Reaper qualified as VMReaper
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
    , cliLogRequests = logRequests
    , cliLogVMs = logVMs
    , cliCommandTimeoutDefault = commandTimeoutDefault
    , cliCommandTimeoutMax = commandTimeoutMax
    , cliStaleVMTimeout = staleVMTimeout
    } <-
    Opt.execParser commandLineParserInfo
  Monad.when (commandTimeoutDefault < 1) $
    error "--command-timeout-default must be at least 1 second."
  Monad.when (commandTimeoutMax < commandTimeoutDefault) $
    error "--command-timeout-max must be at least --command-timeout-default."
  Monad.when (staleVMTimeout < 1) $
    error "--stale-vm-timeout must be at least 1 second."
  vms <- initializeVMPool $ Map.keys executablesMap
  VMReaper.startStaleVMReaper vms staleVMTimeout
  let httpConfig = MCP.Server.defaultHttpConfig {MCP.Server.httpHost = cliHttpHost, MCP.Server.httpPort = cliHttpPort}
  let environment = Internal.MkHandlerEnvironment {..}
  let tool ctx request = Internal.runHandlerT environment (handleTool ctx request)
  let handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''Tool 'tool)}
  MCP.Server.runMcpServerHttpWithConfig httpConfig info handlers
