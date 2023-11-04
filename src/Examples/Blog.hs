module Examples.Blog (start) where

import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Html
import Concur.Replica.Spa.Router qualified as Router
import Concur.Replica.Spa.Widget (Widget)
import Relude hiding (div)


start :: IO ()
start = do
  Spa.start $ Router.widget widget

widget :: Text -> Widget a
widget path = do
  div
    []
    [ div [] [Router.link "Blog", Router.link "About"],
      div [] [text "Welcome to ", text path]
    ]
