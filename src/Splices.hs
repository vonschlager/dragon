{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    ) where

import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Heist.Interpreted
import Snap.Snaplet

import Application
import Db
import Utils

postsSplice :: Splice (Handler App App)
postsSplice = do
    posts <- lift $ with db getAllPosts
    mapSplices renderOption posts
  where
    renderOption :: DbPost -> Splice (Handler App App)
    renderOption p = runChildrenWithText
        [ ("postid", showAsText $ fromMaybe 0 $ pId p)
        , ("title", pTitle p)
        ]
