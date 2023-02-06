module Concur.Replica.Spa.Router (widget, link) where

import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.DOM.Props (href)
import Concur.Replica.Spa.Widget
import Control.Concurrent.STM qualified as STM
import Data.Text qualified as Text
import Relude

widget :: (Text -> Widget a) -> Widget a
widget widget' = do
  path <- awaitCurrentClientPath
  routingWidget path widget'

routingWidget :: Text -> (Text -> Widget a) -> Widget b
routingWidget path widget' = do
  putTextLn path
  routeChangeOrWidgetFinished <- Left <$> awaitPathChange <|> Right <$> widget' path
  case routeChangeOrWidgetFinished of
    Left newPath -> routingWidget newPath widget'
    Right _ -> do
      putTextLn "[Router] Restarting Widget"
      routingWidget path widget'

awaitCurrentClientPath :: Widget Text
awaitCurrentClientPath = do
  routingChannel <- asks envRoutingChannel
  callJs "callCallback(arg, window.location.pathname)" $ \path ->
    STM.atomically $ STM.writeTChan routingChannel $ Text.drop 1 path
  awaitPathChange

awaitPathChange :: Widget Text
awaitPathChange = do
  chan <- asks envRoutingChannel
  liftIO $ STM.atomically (STM.readTChan chan)

link :: Text -> Widget a
link s = do
  routeChan <- asks envRoutingChannel
  _ <- node "a" [onClick, href $ "javascript:/*" <> s <> "*/;"] [text s]
  callJs ("window.history.pushState({}, \"\", '" <> s <> "');") $ \(_ :: Text) -> pure ()
  liftIO . STM.atomically $ STM.writeTChan routeChan s
  link s
