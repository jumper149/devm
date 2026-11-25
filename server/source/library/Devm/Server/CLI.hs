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
    Opt.option
      Opt.auto
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
  pure MkCommandLine {..}

commandLineParserInfo :: Opt.ParserInfo CommandLine
commandLineParserInfo =
  Opt.info (Opt.helper <*> commandLineParser) $
    Opt.fullDesc
      <> Opt.progDesc "Run the devm MCP server."
      <> Opt.header "devm-server"
