{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post(..)
    , getPosts
    , getPost
    , savePost
    , deletePost
    , Navbar(..)
    , getNavbar
    , saveNavbar
    , deleteNavbar
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Text (Text)
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application

data Post = Post
    { postid :: Maybe Integer
    , title :: Text
    , body :: Text
    , time :: UTCTime
    }

data Navbar = Navbar
    { entryid :: Maybe Integer
    , name :: Text
    , kind :: Text
    , order :: Integer
    }

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field

instance FromRow Navbar where
    fromRow = Navbar <$> field <*> field <*> field <*> field

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

saveNavbar :: Navbar -> Handler App Sqlite ()
saveNavbar n = do
    execute "INSERT INTO navbar (name,kind,order) VALUES(?,?,?)"
        (name n, kind n, order n)

deleteNavbar :: Integer -> Handler App Sqlite ()
deleteNavbar i =
    execute "DELETE FROM navbar WHERE id = ?" [i]

getNavbar :: Handler App Sqlite [Navbar]
getNavbar =
    query_ "SELECT id,name,kind,order FROM navbar ORDER BY order DESC"

