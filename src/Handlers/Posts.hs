{-# LANGUAGE OverloadedStrings #-}

module Handlers.Posts
    ( handlePosts
    , handlePost
    , handlePostAdd
    , handlePostEdit
    , handlePostDelete
    ) where

import Heist.Interpreted
import Snap.Snaplet

import Application
import Db
import Utils

mkPostSplice :: Post -> Splice AppHandler
mkPostSplice p = runChildrenWithText
    [ ("posttitle", title p)
    , ("postbody", body p)
    , ("time", showAsText $ time p)
    ]

handlePosts :: Handler App App ()
handlePosts = undefined

handlePost :: Handler App App ()
handlePost = undefined

handlePostAdd :: Handler App App ()
handlePostAdd = undefined

handlePostEdit :: Handler App App ()
handlePostEdit = undefined

handlePostDelete :: Handler App App ()
handlePostDelete = undefined
