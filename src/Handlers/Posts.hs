{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handleNews
    , handlePostView
    ) where

import Data.Maybe (fromMaybe)
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Db
import Utils

renderPost :: DbPost -> Splice (Handler App App)
renderPost p = runChildrenWithText
    [ ("postid", showAsText $ fromMaybe 0 $ postid p)
    , ("title", title p)
    , ("body", body p)
    , ("creation", showAsText $ creation p)
    , ("publish", showAsText $ publish p)
    ]

handleNews :: Handler App App ()
handleNews = do
    news <- with db $ getPostKind "wiesc"
    heistLocal (splices news) $ render "/news"
  where
    splices ns = bindSplices [("news", mapSplices renderPost ns)]

handlePostView :: Handler App App ()
handlePostView = do
    mid <- getParam "postid"
    case mid of
        Nothing -> writeBS "error"
        Just lid -> do
            post <- with db $ getPost $ bs2integer lid
            heistLocal (splice post) $ render "/post-view"
  where
    splice p = bindSplices [("post", renderPost p)]
