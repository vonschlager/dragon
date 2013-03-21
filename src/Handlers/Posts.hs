{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handlePosts
    , handlePostAdd
    , handlePostView
    , handlePostEdit
    , handlePostDelete
    ) where

import Control.Applicative
import Control.Monad.Trans
import Data.Time
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Db
import Utils

handlePosts :: Handler App App ()
handlePosts = do
    posts <- with db getPosts
    heistLocal (splices posts) $ render "/index"
  where
    splices ps = bindSplices [("posts", mapSplices renderPost ps)]
    renderPost :: Post -> Splice AppHandler
    renderPost p = runChildrenWithText
        [ ("title", title p)
        , ("body", body p)
        , ("slug", slug p)
        , ("time", showAsText $ time p)
        ]

handlePostView :: Handler App App ()
handlePostView = undefined

handlePostAdd :: Handler App App ()
handlePostAdd =
    method GET renderPostAddForm <|> method POST handlePostSubmit
  where
    renderPostAddForm = render "/postaddform"      
    handlePostSubmit = do
        mtitle <- getPostParam "title"
        mbody <- getPostParam "body"
        mslug <- getPostParam "slug"
        ltime <- liftIO getCurrentTime
        case sequence [mtitle, mbody, mslug] of
            Nothing -> writeBS "error"
            (Just [ltitle, lbody, lslug]) -> do
                let post = Post Nothing
                                (bs2text ltitle)
                                (bs2text lbody)
                                (bs2text lslug)
                                ltime
                with db $ savePost post
                redirect "/posts"

handlePostEdit :: Handler App App ()
handlePostEdit = undefined

handlePostDelete :: Handler App App ()
handlePostDelete = undefined
