{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.CommandLine where

import Data.Kind
import Data.Map qualified as Map
import Devm.Server.VM
import Options.Applicative qualified as Opt

type CommandLine :: Type
data CommandLine = MkCommandLine
  { cliVMExecutables :: VMExecutables
  }

commandLineParser :: Opt.Parser CommandLine
commandLineParser =
  MkCommandLine . MkVMExecutables . Map.fromList . zip (MkVMID <$> [1 ..])
    <$> Opt.some
      ( MkVMExecutable
          <$> Opt.strOption
            ( Opt.long "vm"
                <> Opt.metavar "EXECUTABLE"
                <> Opt.help "Path to the executable of a VM. Repeat once for each VM."
            )
      )

commandLineParserInfo :: Opt.ParserInfo CommandLine
commandLineParserInfo =
  Opt.info (Opt.helper <*> commandLineParser) $
    Opt.fullDesc
      <> Opt.progDesc "Run the devm MCP server."
      <> Opt.header "devm-server"
