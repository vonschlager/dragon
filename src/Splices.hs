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

sidenavSplice :: Text -> Splice (Handler App App)
sidenavSplice year = do
    sidenav <- lift $ with db getSideNav
    mapSplices renderSideNav sidenav
  where
    renderSideNav :: DbSideNav -> Splice (Handler App App)
    renderSideNav sn = runChildrenWith
        [ ("year", textSplice $ snyear sn) 
        , ("months", mapSplices monthSplice $ snmonths sn)
        , ("in", textSplice $ if' (year == snyear sn) "in" "")
        ]
    monthSplice :: Text -> Splice (Handler App App)
    monthSplice m = runChildrenWithText
        [ ("monthpretty", prettyMonth m)
        , ("month", m)
        ]

navbarSplice :: Splice (Handler App App)
navbarSplice = do
    mapSplices renderMenuItem
        [ ("/wiesci/2012/05", "Wieści")
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
