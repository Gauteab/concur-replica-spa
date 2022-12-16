module Main (main) where

import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Widget
import Relude

main :: IO ()
main = do
  Spa.start widget

widget :: Widget a
widget = do
  initRoute
  _ <- button [onClick] [text "hei"]
  consoleLog "Hello from replica"
  text "Hello!"
