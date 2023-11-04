{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Examples.WorkoutLogger (start) where

import Concur.Replica (className)
import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Html
import Concur.Replica.Spa.Router qualified as Router
import Concur.Replica.Spa.Widget (Widget)
import Control.Concurrent (threadDelay)
import Data.UnixTime qualified as Time
import Relude hiding (div)
import Relude qualified as Relude

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
  e <- div [] (exerciseWidget <$> exercisesSorted)
  let newExercises = e : (exercises & filter (\e' -> name e /= name e'))
  widget newExercises path

data Exercise = Exercise
  { name :: Text,
    timestamp :: Maybe Time.UnixTime
  }
  deriving (Eq, Show)

newExercise :: Text -> Exercise
newExercise name = Exercise {name = name, timestamp = Nothing}

exerciseWidget :: Exercise -> Widget Exercise
exerciseWidget e = do
  _ <-
    div
      [className "exercise"]
      [ text e.name,
        div [] [e.timestamp & maybeWidget viewTimestamp],
        button [onClick] [text "Log"]
      ]
  timeLogged <- liftIO Time.getUnixTime
  pure (e {timestamp = Just timeLogged})

viewTimestamp :: Time.UnixTime -> Widget a
viewTimestamp timestamp = forever $ do
  current <- liftIO Time.getUnixTime
  let diff = Time.diffUnixTime current timestamp
      seconds = diff.udtMicroSeconds `Relude.div` 1000
      (days, secondsWithoutDays) = seconds `divMod` 86400
      (hours, secondsWithoutHours) = secondsWithoutDays `divMod` 3600
      (minutes, remainingSeconds) = secondsWithoutHours `divMod` 60
      timeString = show days <> ":" <> show hours <> ":" <> show minutes <> ":" <> show remainingSeconds
  text timeString <|> liftIO (threadDelay 1000000)
