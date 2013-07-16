{-# LANGUAGE OverloadedStrings #-}

module Splices
    ( postsSplice
    , newsSideNavSplice
    , gbookSideNavSplice
    , navBarSplice
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Heist
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Text.XmlHtml (Node)
import qualified Text.XmlHtml as X

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

newsSideNavSplice :: (Text, Text) -> Splice (Handler App App)
newsSideNavSplice (year, month) = do
    sidenav <- lift $ with db getNewsSideNav
    mapSplices renderNewsSideNav sidenav
  where
    renderNewsSideNav :: DbYMSideNav -> Splice (Handler App App)
    renderNewsSideNav sn = runChildrenWith
        [ ("year", textSplice $ nsnYear sn) 
        , ("months", mapSplices monthSplice $
                        zip (repeat $ nsnYear sn) $ nsnMonths sn)
        , ("in", textSplice $ if' (year == nsnYear sn) "in" "")
        ]
    monthSplice :: (Text, Text) -> Splice (Handler App App)
    monthSplice (y, m) = do
        nodes <- runChildrenWithText
            [ ("monthpretty", prettyMonth m)
            , ("month", m)
            ]
        return $ if month == m && year == y
            then mkElement "li" [("class","active")] nodes
            else mkElement "li" [] nodes

gbookSideNavSplice :: (Text, Text) -> Splice (Handler App App)
gbookSideNavSplice (year, month) = do
    sidenav <- lift $ with db getGBookSideNav
    mapSplices renderNewsSideNav sidenav
  where
    renderNewsSideNav :: DbYMSideNav -> Splice (Handler App App)
    renderNewsSideNav sn = runChildrenWith
        [ ("year", textSplice $ nsnYear sn) 
        , ("months", mapSplices monthSplice $
                        zip (repeat $ nsnYear sn) $ nsnMonths sn)
        , ("in", textSplice $ if' (year == nsnYear sn) "in" "")
        ]
    monthSplice :: (Text, Text) -> Splice (Handler App App)
    monthSplice (y, m) = do
        nodes <- runChildrenWithText
            [ ("monthpretty", prettyMonth m)
            , ("month", m)
            ]
        return $ if month == m && year == y
            then mkElement "li" [("class","active")] nodes
            else mkElement "li" [] nodes

navBarSplice :: Splice (Handler App App)
navBarSplice = do
    rq <- getRequest
    let ruri = bs2t $ rqURI rq
    mapSplices (renderMenuItem ruri)
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
    renderMenuItem :: Text -> (Text, Text) -> Splice (Handler App App)
    renderMenuItem ruri (uri, name) = do
        nodes <- runChildrenWithText
            [ ("url", uri)
            , ("name", name)
            ]
        return $ if uri `T.isPrefixOf` ruri
            then mkElement "li" [("class","active")] nodes
            else mkElement "li" [] nodes
