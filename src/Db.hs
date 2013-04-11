{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post(..)
    , getPostKind
    , getPost
    , getAllPosts
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
    , kind :: Text
    , time :: UTCTime
    }

data Navbar = Navbar
    { entryid :: Maybe Integer
    , name :: Text
    , link :: Text
    , order :: Integer
    }

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field <*> field

instance FromRow Navbar where
    fromRow = Navbar <$> field <*> field <*> field <*> field

savePost :: Post -> Handler App Sqlite ()
savePost p = do
    execute "INSERT INTO posts (title,body,kind,time) VALUES(?,?,?,?)"
        (title p, body p, kind p, time p)

deletePost :: Integer -> Handler App Sqlite ()
deletePost i =
    execute "DELETE FROM posts WHERE id = ?" [i]

getPostKind :: Text -> Handler App Sqlite [Post]
getPostKind k =
    query "SELECT id,title,body,kind,time FROM posts WHERE kind = ?" [k]

getPost :: Integer -> Handler App Sqlite Post
getPost i =
    liftM head $ query "SELECT id,title,body,kind,time FROM posts WHERE id = ?" [i]

getAllPosts :: Handler App Sqlite [Post]
getAllPosts =
    query_ "SELECT id,title,body,kind,time FROM posts"

saveNavbar :: Navbar -> Handler App Sqlite ()
saveNavbar n = do
    execute "INSERT INTO navbar (name,link,ord) VALUES(?,?,?)"
        (name n, link n, order n)

deleteNavbar :: Integer -> Handler App Sqlite ()
deleteNavbar i =
    execute "DELETE FROM navbar WHERE id = ?" [i]

getNavbar :: Handler App Sqlite [Navbar]
getNavbar =
    query_ "SELECT id,name,link,ord FROM navbar"

