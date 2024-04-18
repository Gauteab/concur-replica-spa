{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Examples.WorkoutLogger (start) where

import Concur.Replica (className, style)
import Concur.Replica.DOM.Events (onClick)
import Concur.Replica.Spa qualified as Spa
import Concur.Replica.Spa.Html
import Concur.Replica.Spa.JS qualified as JS
import Concur.Replica.Spa.Router qualified as Router
import Concur.Replica.Spa.Widget (Widget)
import Control.Concurrent (threadDelay)
import Data.UnixTime qualified as Time
import Relude hiding (div)
import Relude qualified

start :: IO ()
start = do
  Spa.start $ Router.widget (widget exerciseList)

exerciseList :: [Exercise]
exerciseList =
  [ newExercise "Pushups" (2, 3),
    newExercise "Pullups" (1, 2)
  ]

widget :: [Exercise] -> Text -> Widget a
widget exercises path = do
  let exercisesSorted = sortOn (Down . timestamp) exercises
  e <- div [] (exerciseWidget <$> exercisesSorted)
  let newExercises = e : filter (/= e) exercises
  widget newExercises path

data Exercise = Exercise
  { name :: Text,
    timestamp :: Maybe Time.UnixTime,
    frequency :: Frequency,
    status :: FrequencyStatus
  }
  deriving (Show)

instance Eq Exercise where
  e1 == e2 = e1.name == e2.name

data Frequency = Frequency
  { minFrequency :: Int,
    maxFrequency :: Int
  }
  deriving (Eq, Show)

newExercise :: Text -> (Int, Int) -> Exercise
newExercise name (minFrequency, maxFrequency) =
  Exercise
    { name = name,
      timestamp = Nothing,
      frequency = Frequency {minFrequency = minFrequency, maxFrequency = maxFrequency},
      status = Red
    }

exerciseWidget :: Exercise -> Widget Exercise
exerciseWidget e = do
  div
    [ classNames
        [ ("exercise", True),
          ("exercise-green", e.status == Green),
          ("exercise-red", e.status == Red)
        ]
    ]
    [ text e.name,
      timestampWidget e,
      logButton e
    ]

data FrequencyStatus = Green | Yellow | Red deriving (Eq, Show)

frequencyStatus :: Int -> Exercise -> FrequencyStatus
frequencyStatus seconds e
  | seconds <= e.frequency.minFrequency = Green
  | seconds <= e.frequency.maxFrequency = Yellow
  | otherwise = Red

logButton :: Exercise -> Widget Exercise
logButton e = do
  _ <- button [onClick] [text "Log"]
  timeLogged <- liftIO Time.getUnixTime
  pure (e {timestamp = Just timeLogged})

timestampWidget :: Exercise -> Widget Exercise
timestampWidget e = do
  -- JS.call_ "const d = new Date(); document.getElementById('demo').innerHTML = d.toLocaleTimeString();"
  case e.timestamp of
    Nothing -> text ""
    Just timestamp -> do
      current <- liftIO Time.getUnixTime
      let diff = Time.diffUnixTime current timestamp
          seconds = diff.udtMicroSeconds `Relude.div` 1000
          (days, secondsWithoutDays) = seconds `divMod` 86400
          (hours, secondsWithoutHours) = secondsWithoutDays `divMod` 3600
          (minutes, remainingSeconds) = secondsWithoutHours `divMod` 60
          timeString = show days <> ":" <> show hours <> ":" <> show minutes <> ":" <> show remainingSeconds
          status = frequencyStatus (fromIntegral seconds) e
      _ <- text timeString <|> liftIO (threadDelay 1000000)
      pure (e {status = status})
