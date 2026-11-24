{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.CLI where

import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Devm.Server.VM
import Options.Applicative qualified as Opt

type CommandLine :: Type
data CommandLine = MkCommandLine
  { cliVMExecutables :: VMExecutables
  , cliReadyMarker :: Text.Text
  , cliSSHKey :: FilePath
  }

commandLineParser :: Opt.Parser CommandLine
commandLineParser =
  MkCommandLine
    . MkVMExecutables
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
    <*> Opt.strOption
      ( Opt.long "ready"
          <> Opt.metavar "MARKER"
          <> Opt.help "Log VM stdout until a line contains MARKER, then discard it."
          <> Opt.value ""
      )
    <*> Opt.strOption
      ( Opt.long "ssh-key"
          <> Opt.metavar "FILE"
          <> Opt.help "Path to the SSH private key used when sending commands to VMs."
      )

commandLineParserInfo :: Opt.ParserInfo CommandLine
commandLineParserInfo =
  Opt.info (Opt.helper <*> commandLineParser) $
    Opt.fullDesc
      <> Opt.progDesc "Run the devm MCP server."
      <> Opt.header "devm-server"
