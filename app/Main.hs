module Main (main) where

-- import Examples.Blog qualified as Blog
import Examples.WorkoutLogger qualified as WorkoutLogger
import Relude

main :: IO ()
main = WorkoutLogger.start
