{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    , sideNavSplice
    , navBarSplice
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
    mapSplices renderMenuItem
        [ ("/wiesci", "Wieści")
        , ("/bohater", "Bohater")
        , ("/historia", "Historia")
        , ("/sklad", "Skład")
        , ("/imprezy", "Imprezy")
        , ("/zapalka", "Zapałka")
        , ("/ksiega", "Księga gości")
        , ("/galeria", "Galeria")
        , ("/konstytucja", "Konstytucja")
        , ("/onas", "O nas")
        ]
  where
    renderMenuItem :: (Text, Text) -> Splice (Handler App App)
    renderMenuItem (url, name) = runChildrenWithText
        [ ("url", url)
        , ("name", name)
        ]
