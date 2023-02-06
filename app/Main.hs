module Main (main) where

-- import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Router qualified as Router
import Concur.Replica.Spa.Widget
import Relude hiding (div)

main :: IO ()
main = do
  Spa.start $ Router.widget widget

widget :: Text -> Widget a
widget path = do
  div
    []
    [ div [] [Router.link "Blog", Router.link "About"],
      div [] [text "Welcome to ", text path]
    ]
