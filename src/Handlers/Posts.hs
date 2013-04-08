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
import Data.Maybe (fromMaybe)
import Data.Time
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Db
import Utils

renderPost :: Post -> Splice (Handler App App)
renderPost p = runChildrenWithText
    [ ("postid", showAsText $ fromMaybe 0 $ postid p)
    , ("title", title p)
    , ("body", body p)
    , ("time", showAsText $ time p)
    ]

handlePosts :: Handler App App ()
handlePosts = do
    posts <- with db getPosts
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
            heistLocal (splice post) $ render "/postview"
  where
    splice p = bindSplices [("post", renderPost p)]

handlePostAdd :: Handler App App ()
handlePostAdd =
    method GET renderPostAddForm <|> method POST handlePostSubmit
  where
    renderPostAddForm = render "/postaddform"      
    handlePostSubmit = do
        mtitle <- getPostParam "title"
        mbody <- getPostParam "body"
        ltime <- liftIO getCurrentTime
        case sequence [mtitle, mbody] of
            Nothing -> writeBS "error"
            (Just [ltitle, lbody]) -> do
                let post = Post Nothing
                                (bs2text ltitle)
                                (bs2text lbody)
                                ltime
                with db $ savePost post
                redirect "/posts"

handlePostEdit :: Handler App App ()
handlePostEdit = undefined

handlePostDelete :: Handler App App ()
handlePostDelete = do
    mid <- getParam "postid"
    case mid of
        Nothing -> writeBS "error"
        Just lid -> do
            with db $ deletePost $ bs2integer lid
            redirect "/posts"
