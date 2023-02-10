module Concur.Replica.Spa.Router (widget, link) where

import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.DOM.Props (href)
import Concur.Replica.Spa.Html (a, text)
import Concur.Replica.Spa.JS qualified as JS
import Concur.Replica.Spa.Widget
import Control.Concurrent.STM qualified as STM
import Data.Text qualified as Text
import Relude

widget :: (Text -> Widget a) -> Widget a
widget widget' = do
  path <- getCurrentClientPath
  setOnPopState
  routingWidget path widget'

routingWidget :: Text -> (Text -> Widget a) -> Widget b
routingWidget path widget' = do
  putTextLn ("[Router] " <> path)
  routeChangeOrWidgetFinished <- Left <$> awaitPathChange <|> Right <$> widget' path
  case routeChangeOrWidgetFinished of
    Left newPath -> routingWidget newPath widget'
    Right _ -> do
      putTextLn "[Router] Restarting Widget"
      routingWidget path widget'

getCurrentClientPath :: Widget Text
getCurrentClientPath = do
  JS.eval "location.pathname" <&> Text.drop 1

setOnPopState :: Widget ()
setOnPopState = do
  chan <- useRoutingChannel
  JS.call "window.addEventListener('popstate', (event) => { callCallback(arg, location.pathname); console.log('test') })" $ \path -> do
    putTextLn "[Router] PopState"
    STM.atomically $ STM.writeTChan chan path

awaitPathChange :: Widget Text
awaitPathChange = do
  chan <- useRoutingChannel
  liftIO $ STM.atomically (STM.readTChan chan)

link :: Text -> Widget a
link s = do
  routeChan <- useRoutingChannel
  _ <- a [onClick, href $ "javascript:/*" <> s <> "*/;"] [text s]
  JS.call_ ("window.history.pushState({}, \"\", '" <> s <> "');")
  liftIO . STM.atomically $ STM.writeTChan routeChan s
  link s
