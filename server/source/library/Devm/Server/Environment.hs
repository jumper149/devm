{-# LANGUAGE NoFieldSelectors #-}

module Devm.Server.Environment where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
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

type HandlerM :: Type -> Type
newtype HandlerM a = MkHandlerM {unHandlerM :: ReaderT HandlerEnvironment IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runHandlerM :: HandlerEnvironment -> HandlerM a -> IO a
runHandlerM env MkHandlerM {unHandlerM} = runReaderT unHandlerM env

getEnvironment :: HandlerM HandlerEnvironment
getEnvironment = MkHandlerM ask
