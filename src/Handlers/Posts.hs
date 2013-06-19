{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handlePostKind
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

handlePostKind :: Handler App App ()
handlePostKind = do
    mkind <- getParam "kind"
    case mkind of
        Nothing -> writeBS "error"
        Just lkind -> do
            posts <- with db $ getPostKind $ bs2text lkind
            heistLocal (splices posts) $ render "/posts"
  where
    splices ps = bindSplices [("posts", mapSplices renderPost ps)]

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
