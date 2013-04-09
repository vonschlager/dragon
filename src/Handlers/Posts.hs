{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handlePostKind
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
        mkind <- getPostParam "kind"
        ltime <- liftIO getCurrentTime
        case sequence [mtitle, mbody, mkind] of
            Nothing -> writeBS "error"
            (Just [ltitle, lbody, lkind]) -> do
                let post = Post Nothing
                                (bs2text ltitle)
                                (bs2text lbody)
                                (bs2text lkind)
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
