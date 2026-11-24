{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Environment where

import Control.Concurrent.STM qualified as STM
import Data.Kind
import Data.Text qualified as Text
import Devm.Server.VM

type HandlerEnvironment :: Type
data HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
  , executables :: VMExecutables
  , readyMarker :: Text.Text
  , sshKey :: FilePath
  }
