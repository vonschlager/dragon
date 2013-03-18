{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handlePosts
    , handlePost
    , handlePostAdd
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

renderPost :: Post -> Splice AppHandler
renderPost p = runChildrenWithText
    [ ("title", title p)
    , ("body", body p)
    , ("slug", slug p)
    , ("time", showAsText $ time p)
    ]

handlePosts :: Handler App App ()
handlePosts = do
    posts <- with db getPosts
    heistLocal (splices posts) $ render "/index"
  where
    splices ps = bindSplices [("posts", mapSplices renderPost ps)]

handlePost :: Handler App App ()
handlePost = undefined

handlePostAdd :: Handler App App ()
handlePostAdd =
    method GET renderPostAddForm <|> method POST handlePostSubmit
  where
    renderPostAddForm = render "postadd"      
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

handlePostEdit :: Handler App App ()
handlePostEdit = undefined

handlePostDelete :: Handler App App ()
handlePostDelete = undefined
