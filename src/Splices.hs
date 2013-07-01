{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    , sidenavSplice
    ) where

import Control.Monad.Trans
import Data.Text (Text)
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

sidenavSplice :: Text -> Splice (Handler App App)
sidenavSplice year = do
    sidenav <- lift $ with db getSideNav
    mapSplices renderSidenav sidenav
  where
    renderSidenav :: DbSideNav-> Splice (Handler App App)
    renderSidenav sn = runChildrenWith
        [ ("year", textSplice $ snyear sn) 
        , ("months", mapSplices monthSplice $ snmonths sn)
        , ("in", textSplice $ if' (year == snyear sn) "in" "")
        ]
    monthSplice :: Text -> Splice (Handler App App)
    monthSplice m = runChildrenWithText
        [ ("monthpretty", prettyMonth m)
        , ("month", m)
        ]
