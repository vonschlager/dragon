{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post (..)
    , getPosts
    , getPost
    , savePost
    , deletePost
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application
import Utils (showAsText, if')

data Post = Post
    { postid :: Maybe Integer
    , title :: Text
    , body :: Text
    , time :: UTCTime
    }

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field

savePost :: Post -> Handler App Sqlite ()
savePost p = do
    execute "INSERT INTO posts (title,body,time) VALUES(?,?,?)"
        (title p, body p, time p)

deletePost :: Integer -> Handler App Sqlite ()
deletePost i =
    execute "DELETE FROM posts WHERE id = ?" [i]

getPosts :: Handler App Sqlite [Post]
getPosts =
    query_ "SELECT id,title,body,time FROM posts ORDER BY time DESC"

getPost :: Integer -> Handler App Sqlite Post
getPost i =
    liftM head $ query "SELECT id,title,body,time FROM posts WHERE id = ?" [i]
