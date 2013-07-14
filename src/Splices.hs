{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    , sideNavSplice
    , navBarSplice
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted
import Snap.Core
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

sideNavSplice :: (Text, Text) -> Splice (Handler App App)
sideNavSplice (year, month) = do
    sidenav <- lift $ with db getNewsSideNav
    mapSplices renderNewsSideNav sidenav
  where
    renderNewsSideNav :: DbNewsSideNav -> Splice (Handler App App)
    renderNewsSideNav sn = runChildrenWith
        [ ("year", textSplice $ nsnYear sn) 
        , ("months", mapSplices monthSplice $
                        zip (repeat $ nsnYear sn) $ nsnMonths sn)
        , ("in", textSplice $ if' (year == nsnYear sn) "in" "")
        ]
    monthSplice :: (Text, Text) -> Splice (Handler App App)
    monthSplice (y, m) = runChildrenWithText
        [ ("monthpretty", prettyMonth m)
        , ("month", m)
        , ("active", if' (month == m && year == y) "active" "")
        ]

navBarSplice :: Splice (Handler App App)
navBarSplice = do
    rq <- getRequest
    let ruri = bs2t $ rqURI rq
    
    mapSplices renderMenuItem
        [ (ruri, "/wiesci", "Wieści")
        , (ruri, "/bohater", "Bohater")
        , (ruri, "/historia", "Historia")
        , (ruri, "/sklad", "Skład")
        , (ruri, "/imprezy", "Imprezy")
        , (ruri, "/zapalka", "Zapałka")
        , (ruri, "/ksiega", "Księga gości")
        , (ruri, "/galeria", "Galeria")
        , (ruri, "/konstytucja", "Konstytucja")
        , (ruri, "/onas", "O nas")
        ]
  where
    renderMenuItem :: (Text, Text, Text) -> Splice (Handler App App)
    renderMenuItem (rurl, url, name) = runChildrenWithText
        [ ("active", if' (url `T.isPrefixOf` rurl) "active" "")
        , ("url", url)
        , ("name", name)
        ]
