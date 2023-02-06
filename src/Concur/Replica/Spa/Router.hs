module Concur.Replica.Spa.Router (widget, link) where
  
import Concur.Replica.Spa.Widget
import Relude
import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.DOM.Props (href)
import Data.Text qualified as Text
import Control.Concurrent.STM qualified as STM

widget :: (Text -> Widget b) -> Widget b
widget w = do
    path <- getinitRoute
    routingWidget path w

routingWidget :: Text -> (Text -> Widget a) -> Widget a
routingWidget path w = do
  putTextLn path
  routeChangeOrWidgetFinished <- Left <$> useHistory <|> Right <$> w path
  case routeChangeOrWidgetFinished of
    Left newPath -> routingWidget newPath w
    Right _ -> do
      putTextLn "Restarting Widget"
      w path

    
-- initRoute :: Widget ()
-- initRoute = do
--   callJs "callCallback(arg, window.location.pathname)" $
--     \path -> putStrLn ("Hello from " <> path)

getinitRoute :: Widget Text
getinitRoute = do
  routeChan <- asks envRoute
  callJs "callCallback(arg, window.location.pathname)" $ \path -> 
    STM.atomically $ STM.writeTChan routeChan $ Text.drop 1 path
  useHistory

useHistory :: Widget Text
useHistory = do 
  chan <- asks envRoute 
  liftIO $ STM.atomically (STM.readTChan chan)


link :: Text -> Widget ()
link s = do
  routeChan <- asks envRoute
  _ <- node "a" [onClick, href $ "javascript:/*" <> s <> "*/;"] [text s]
  callJs ("window.history.pushState({}, \"\", '" <> s <> "');") $ \(_ :: Text) -> pure ()
  liftIO . STM.atomically $ STM.writeTChan routeChan s
  link s




