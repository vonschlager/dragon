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
    heistLocal (splices posts) $ render "/posts"
  where
    splices ps = bindSplices [("posts", mapSplices renderPost ps)]

handlePostView :: Handler App App ()
handlePostView = do
    mslug <- getParam "slug"
    case mslug of
        Nothing -> writeBS "error"
        Just lslug -> do
            post <- with db $ getPost $ bs2text lslug
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
                                (mkSlug $ bs2text ltitle)
                                ltime
                with db $ savePost post
                redirect "/posts"

handlePostEdit :: Handler App App ()
handlePostEdit = undefined

handlePostDelete :: Handler App App ()
handlePostDelete = undefined
