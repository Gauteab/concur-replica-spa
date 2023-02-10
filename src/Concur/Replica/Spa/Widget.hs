{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concur.Replica.Spa.Widget (Env (..), Widget, runWidget, useReplicaContext, new, useRoutingChannel) where

import Concur.Core qualified as Core
import Control.Concurrent.STM qualified as STM
import Network.Wai.Handler.Replica qualified as Replica
import Relude hiding (div)
import Replica.VDOM.Types qualified as Types

newtype Widget a = Widget (ReaderT Env (Core.Widget Types.HTML) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, Alternative)

data Env = Env
  { envContext :: Replica.Context,
    envRoutingChannel :: STM.TChan Text
  }

new :: Core.Widget Types.HTML a -> Widget a
new = Widget . lift

runWidget :: Env -> Widget a -> Core.Widget Types.HTML a
runWidget ctx (Widget r) = runReaderT r ctx

useReplicaContext :: Widget Replica.Context
useReplicaContext = asks envContext

useRoutingChannel :: Widget (STM.TChan Text)
useRoutingChannel = asks envRoutingChannel
