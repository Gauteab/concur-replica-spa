{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concur.Replica.Spa.Widget (Widget, runWidget, node, useReplicaContext, button, div, text, callJs, initRoute, consoleLog, nothing) where

import Concur.Core qualified as Core
import Concur.Replica.DOM qualified as DOM
import Concur.Replica.DOM.Props (Props)
import Data.Aeson (FromJSON)
import Network.Wai.Handler.Replica as Replica
import Relude hiding (div)
import Replica.VDOM.Types qualified as Types

newtype Widget a = Widget (ReaderT Context (Core.Widget Types.HTML) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

new :: Core.Widget Types.HTML a -> Widget a
new = Widget . lift

runWidget :: Context -> Widget a -> Core.Widget Types.HTML a
runWidget ctx (Widget r) = runReaderT r ctx

node :: Text -> [Props a] -> [Widget a] -> Widget a
node tag props children = do
  ctx <- useReplicaContext
  new $ DOM.el tag props (runWidget ctx <$> children)

text :: Text -> Widget a
text = new . DOM.text

useReplicaContext :: Widget Context
useReplicaContext = ask

callJs :: FromJSON a => Text -> (a -> IO ()) -> Widget ()
callJs code callback = do
  ctx <- useReplicaContext
  cb <- liftIO $ registerCallback ctx callback
  liftIO $ Replica.call ctx cb code

initRoute :: Widget ()
initRoute = do
  callJs "callCallback(arg, window.location.pathname)" $
    \path -> putStrLn ("Hello from " <> path)

consoleLog :: Text -> Widget ()
consoleLog message =
  callJs ("console.log(\"" <> message <> "\")") noCallback

noCallback :: Text -> IO ()
noCallback _ = pure ()

--- HTML helpers

nothing :: Widget a
nothing = text ""

div :: [Props a] -> [Widget a] -> Widget a
div = node "div"

button :: [Props a] -> [Widget a] -> Widget a
button = node "button"

-- link = do
--   e <- node "a" [onClick] [text "hello"]
