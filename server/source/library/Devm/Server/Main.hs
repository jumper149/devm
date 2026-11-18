{-# LANGUAGE TemplateHaskell #-}

module Devm.Server.Main where

import Data.Kind
import Data.Text
import MCP.Server qualified
import MCP.Server.Derive qualified

type Units :: Type
data Units = Celsius | Fahrenheit

type WeatherTool :: Type
data WeatherTool
  = CurrentWeather {city :: Text, units :: Maybe Units}

$(pure [])

handleTool :: MCP.Server.ClientContext -> WeatherTool -> IO MCP.Server.Content
handleTool _ (CurrentWeather c _) = pure $ MCP.Server.ContentText $ "Sunny in " <> c

handlers :: MCP.Server.McpServerHandlers
handlers = MCP.Server.noHandlers {MCP.Server.tools = Just $(MCP.Server.Derive.deriveToolHandler ''WeatherTool 'handleTool)}

info :: MCP.Server.McpServerInfo
info =
  MCP.Server.McpServerInfo
    { MCP.Server.serverName = "devm"
    , MCP.Server.serverVersion = "0"
    , MCP.Server.serverInstructions = ""
    }

main :: IO ()
main = MCP.Server.runMcpServerHttp info handlers
