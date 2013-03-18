{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post (..)
    , getPosts
    , savePost
    ) where

import Control.Applicative
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

instance FromRow Post where
    fromRow = Post <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

savePost :: Post -> Handler App Sqlite ()
savePost p = 
    execute "INSERT INTO posts (title,body,slug,time) VALUES(?,?,?,?)"
            (title p, body p, slug p, time p)

getPosts :: Handler App Sqlite [Post]
getPosts =
    query_ "SELECT id,title,body,slug,time FROM posts"
