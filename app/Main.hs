module Main (main) where

-- import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Relude hiding (div)
import Concur.Replica.Spa.Widget 
import Concur.Replica.Spa.Router qualified as Router 

main :: IO ()
main = do
  Spa.start $ Router.widget widget

widget :: Text -> Widget a
widget path = do
  div []
    [ div [] [ Router.link "Blog", Router.link "About" ] 
    , div [] [ text "Welcome to ", text path ]
    ]
