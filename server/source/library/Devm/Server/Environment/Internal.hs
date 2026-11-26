module Devm.Server.Environment.Internal where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind
import Devm.Server.VM

type HandlerEnvironment :: Type
data HandlerEnvironment = MkHandlerEnvironment
  { vms :: STM.TVar VMPool
  , executables :: VMExecutables
  , sshKeyFile :: FilePath
  , logRequests :: Bool
  , logVMs :: Bool
  }

type HandlerT :: (Type -> Type) -> (Type -> Type)
newtype HandlerT m a = MkHandlerT {unHandlerT :: ReaderT HandlerEnvironment m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch)

runHandlerT :: HandlerEnvironment -> HandlerT m a -> m a
runHandlerT env = (`runReaderT` env) . unHandlerT

getEnvironment :: Monad m => HandlerT m HandlerEnvironment
getEnvironment = MkHandlerT ask
