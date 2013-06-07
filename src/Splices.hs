{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( kindsSplice
    , postsSplice
    ) where

import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Heist.Interpreted
import Snap.Snaplet

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
