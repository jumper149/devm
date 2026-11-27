{-# LANGUAGE ApplicativeDo #-}

module Devm.Server.CLI where

import Data.Kind
import Data.Map qualified as Map
import Devm.Server.VM
import Options.Applicative qualified as Opt

type CommandLine :: Type
data CommandLine = MkCommandLine
  { cliVMExecutables :: VMExecutables
  , cliSSHKey :: FilePath
  , cliHttpHost :: String
  , cliHttpPort :: Int
  , cliLogRequests :: Bool
  , cliLogVMs :: Bool
  , cliCommandTimeoutDefault :: Int
  , cliCommandTimeoutMax :: Int
  }

commandLineParser :: Opt.Parser CommandLine
commandLineParser = do
  cliVMExecutables <-
    MkVMExecutables
      . Map.fromList
      . zip (MkVMID <$> [1 ..])
      <$> Opt.some
        ( MkVMExecutable
            <$> Opt.strOption
              ( Opt.long "vm"
                  <> Opt.metavar "EXECUTABLE"
                  <> Opt.help "Path to the executable of a VM. Repeat once for each VM."
              )
        )
  cliSSHKey <-
    Opt.strOption
      ( Opt.long "ssh-key"
          <> Opt.metavar "FILE"
          <> Opt.help "Path to the SSH private key used when sending commands to VMs."
      )
  cliHttpHost <-
    Opt.strOption
      ( Opt.long "host"
          <> Opt.metavar "HOST"
          <> Opt.help "Hostname/IP to listen on."
          <> Opt.value "localhost"
      )
  cliHttpPort <-
    Opt.option
      Opt.auto
      ( Opt.long "port"
          <> Opt.metavar "PORT"
          <> Opt.help "Port to listen on."
          <> Opt.value 9141
      )
  cliLogRequests <-
    Opt.switch
      ( Opt.long "log-requests"
          <> Opt.help "Log requests on StdOut."
      )
  cliLogVMs <-
    Opt.switch
      ( Opt.long "log-vms"
          <> Opt.help "Log VM startup on StdOut."
      )
  cliCommandTimeoutDefault <-
    Opt.option
      Opt.auto
      ( Opt.long "command-timeout-default"
          <> Opt.metavar "SECONDS"
          <> Opt.help "Default wall-clock timeout for commands sent to VMs."
          <> Opt.value 300
      )
  cliCommandTimeoutMax <-
    Opt.option
      Opt.auto
      ( Opt.long "command-timeout-max"
          <> Opt.metavar "SECONDS"
          <> Opt.help "Maximum wall-clock timeout a command request may select."
          <> Opt.value 3600
      )
  pure MkCommandLine {..}

commandLineParserInfo :: Opt.ParserInfo CommandLine
commandLineParserInfo =
  Opt.info (Opt.helper <*> commandLineParser) $
    Opt.fullDesc
      <> Opt.progDesc "Run the devm MCP server."
      <> Opt.header "devm-server"
