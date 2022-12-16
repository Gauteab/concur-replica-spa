{-# LANGUAGE OverloadedLists #-}

module Concur.Replica.Spa (start) where

import Concur.Replica qualified
import Concur.Replica.Spa.Widget
import Network.Wai qualified
import Network.Wai.Middleware.Static qualified
import Network.WebSockets qualified as WebSockets
import Relude hiding (div)
import Replica.VDOM qualified as VDOM

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
      runWidget ctx (initRoute >> widget)

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
