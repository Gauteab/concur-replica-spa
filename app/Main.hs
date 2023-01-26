module Main (main) where

import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Widget
import Relude hiding (div)
import Concur.Replica.Spa.Widget (link, div, getinitRoute)
import GHC.Base (undefined)

main :: IO ()
main = do
  Spa.start $ do
    path <- getinitRoute
    routingWidget path widget

routingWidget :: Text -> (Text -> Widget a) -> Widget a
routingWidget path w = do
  x <- Left <$> useHistory <|> Right <$> w path
  print path
  case x of
    Left path -> routingWidget path w
    Right a -> do
      print "restarting widget"
      w path

widget :: Text -> Widget a
widget path = do
  -- _ <- button [onClick] [text "hei"]
  div []
    [ div [] [ link "Blog", link "About" ] 
    , div [][text "Welcome to ", text path]
    ]
  -- callJs "window.history.pushState({}, \"\", 'hei');" $ \(a :: Void) -> putStrLn "!"
  -- consoleLog "Hello from replica"
  widget path
