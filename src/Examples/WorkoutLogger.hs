{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Examples.WorkoutLogger (start) where

import Concur.Replica (className)
import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Html
import Concur.Replica.Spa.Router qualified as Router
import Concur.Replica.Spa.Widget (Widget)
import Data.Time qualified as Time
import Relude hiding (div)

start :: IO ()
start = do
  Spa.start $ Router.widget (widget exerciseList)

exerciseList :: [Exercise]
exerciseList =
  [ newExercise "Pushups",
    newExercise "Pullups"
  ]

widget :: [Exercise] -> Text -> Widget a
widget exercises path = do
  let exercisesSorted = sortOn (Down . timestamp) exercises
  action <- div [] (fmap exerciseWidget exercisesSorted)
  newExercises <- case action of
    LogExercise e -> do
      timeLogged <- liftIO Time.getCurrentTime
      pure $ exercises <&> (\e' -> (if name e' == name e then (e {timestamp = Just timeLogged}) else e'))
  widget newExercises path

data Exercise = Exercise
  { name :: Text,
    timestamp :: Maybe Time.UTCTime
  }
  deriving (Eq, Show)

data Action = LogExercise Exercise

newExercise :: Text -> Exercise
newExercise name = Exercise {name = name, timestamp = Nothing}

exerciseWidget :: Exercise -> Widget Action
exerciseWidget e = do
  _ <-
    div
      [className "excercise"]
      [ text (name e),
        text (show (timestamp e)),
        button [onClick] [text "Log"]
      ]
  pure (LogExercise e)

-- timeLogged <- liftIO Time.getCurrentTime
-- pure (e {timestamp = Just timeLogged})
