{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    , sidenavSplice
    , navbarSplice
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

sidenavSplice :: (Text, Text) -> Splice (Handler App App)
sidenavSplice (year, month) = do
    sidenav <- lift $ with db getSideNav
    mapSplices renderSideNav sidenav
  where
    renderSideNav :: DbSideNav -> Splice (Handler App App)
    renderSideNav sn = runChildrenWith
        [ ("year", textSplice $ snyear sn) 
        , ("months", mapSplices monthSplice $
                        zip (repeat $ snyear sn) $ snmonths sn)
        , ("in", textSplice $ if' (year == snyear sn) "in" "")
        ]
    monthSplice :: (Text, Text) -> Splice (Handler App App)
    monthSplice (y, m) = runChildrenWithText
        [ ("monthpretty", prettyMonth m)
        , ("month", m)
        , ("active", if' (month == m && year == y) "active" "")
        ]

navbarSplice :: Splice (Handler App App)
navbarSplice = do
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
