{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post (..)
    , savePost
    ) where

import Data.Text
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application

data Post = Post
    { postId :: Maybe Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , time :: UTCTime
    }

savePost :: Post -> Handler App Sqlite ()
savePost p = 
    execute "INSERT INTO posts (title, body, slug, time) VALUES(?, ?, ?, ?)"
            (title p, body p, slug p, time p)
