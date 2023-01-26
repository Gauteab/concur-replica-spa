{-# LANGUAGE OverloadedLists #-}

module Concur.Replica.Spa (start) where

import Concur.Replica qualified
import Network.Wai.Handler.Replica as Replica
import Concur.Replica.Spa.Widget
import Network.Wai qualified
import Network.Wai.Middleware.Static qualified
import Network.WebSockets qualified as WebSockets
import Relude hiding (div)
import Replica.VDOM qualified as VDOM
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

start :: Widget a -> IO ()
start widget = do
  let port = 8083
  putStrLn ("Running on port " <> show port)
  Concur.Replica.run
    port
    index
    WebSockets.defaultConnectionOptions
    static
    $ \ctx -> do
      chan <- liftIO $ newHistoryChan ctx
      let env = Env ctx chan
      runWidget env widget

newHistoryChan :: Replica.Context -> IO (TChan Text)
newHistoryChan ctx = do
  chan <- newTChanIO
  cb <- Replica.registerCallback ctx $ \(path :: Text) -> do
    putStrLn $ toString ("-> " <> path)
    Control.Concurrent.STM.atomically (writeTChan chan path)
  Replica.call ctx cb "window.onpopstate = function(event) { callCallback(arg, location.pathname); };"
  pure chan

-- | Declare static resources like stylesheets
static :: Network.Wai.Middleware
static =
  Network.Wai.Middleware.Static.staticPolicy $
    Network.Wai.Middleware.Static.only
      [ ("static/main.css", "static/main.css")
      ]

-- | Manually build the initial HTML value
index :: Concur.Replica.HTML
index =
  [ VDOM.VLeaf "!doctype" [("html", Concur.Replica.ABool True)] Nothing,
    VDOM.VNode
      "html"
      mempty
      Nothing
      [ VDOM.VNode
          "head"
          mempty
          Nothing
          [ VDOM.VLeaf "meta" [("charset", Concur.Replica.AText "utf-8")] Nothing,
            VDOM.VNode "title" mempty Nothing [VDOM.VText "Concur Replica App"],
            VDOM.VLeaf
              "meta"
              [ ("name", Concur.Replica.AText "viewport"),
                ("content", Concur.Replica.AText "width=device-width, initial-scale=1")
              ]
              Nothing,
            VDOM.VLeaf
              "link"
              [ ("href", Concur.Replica.AText "static/main.css"),
                ("rel", Concur.Replica.AText "stylesheet")
              ]
              Nothing
          ],
        VDOM.VNode
          "body"
          mempty
          Nothing
          [ VDOM.VNode
              "script"
              [("language", Concur.Replica.AText "javascript")]
              Nothing
              [VDOM.VRawText $ decodeUtf8 Concur.Replica.clientDriver]
          ]
      ]
  ]
