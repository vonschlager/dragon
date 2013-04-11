{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( kindsSplice
    , postsSplice
    ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.SqliteSimple
import qualified Text.XmlHtml as X

import Application
import Db
import Utils

kindsSplice :: Splice (Handler App App)
kindsSplice = 
    mapSplices renderOption [("news","News"),("blog","Blog")]
  where
    renderOption :: (Text,Text) -> Splice (Handler App App)
    renderOption k = runChildrenWithText
        [ ("kind", fst k)
        , ("name", snd k)
        ]

postsSplice :: Splice (Handler App App)
postsSplice = do
    posts <- lift $ with db getAllPosts
    mapSplices renderOption posts
  where
    renderOption :: Post -> Splice (Handler App App)
    renderOption p = runChildrenWithText
        [ ("postid", showAsText $ fromMaybe 0 $ postid p)
        , ("title", title p)
        ]
