module Concur.Replica.Spa.JS (call, call_, eval, consoleLog) where

import Concur.Replica.Spa.Widget (Widget, useReplicaContext)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (FromJSON)
import Network.Wai.Handler.Replica qualified as Replica
import Relude hiding (div)

call :: FromJSON a => Text -> (a -> IO ()) -> Widget ()
call code callback = do
  ctx <- useReplicaContext
  cb <- liftIO $ Replica.registerCallback ctx callback
  liftIO $ Replica.call ctx cb code

call_ :: Text -> Widget ()
call_ = flip call noCallback

eval :: FromJSON a => Text -> Widget a
eval code = do
  chan <- liftIO STM.newTChanIO
  call ("callCallback(arg, " <> code <> ")") $ \path ->
    STM.atomically $ STM.writeTChan chan path
  liftIO $ STM.atomically $ STM.readTChan chan

noCallback :: Text -> IO ()
noCallback _ = pure ()

consoleLog :: Text -> Widget ()
consoleLog message =
  call_ ("console.log(\"" <> message <> "\")")
