{-# LANGUAGE OverloadedStrings #-}

module Handlers.News
    ( handleNews
    , handleNewsRange
    , handleNewsByYearMonth
    , handlePostView
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Application
import Db
import Utils

renderPost :: DbPost -> Splice (Handler App App)
renderPost p = runChildrenWith
    [ ("id", textSplice . showAsText $ fromMaybe 0 $ pId p)
    , ("title", textSplice $ pTitle p)
    , ("body", nodes $ pBody p)
    , ("creation", textSplice . showAsText $ pCreation p)
    , ("publish", textSplice . showAsText $ pPublish p)
    ]
  where nodes t = do case X.parseHTML (T.unpack $ pTitle p) $ T.encodeUtf8 t of
                      Left err -> return [X.TextNode $ T.pack err]
                      Right d  -> return $ X.docContent d

handleNews :: Handler App App ()
handleNews = do
    news <- with db $ getPostKind "wiesc"
    heistLocal (splices news) $ render "/news"
  where
    splices ns = bindSplices [("news", mapSplices renderPost ns)]

handleNewsRange :: Handler App App ()
handleNewsRange = do
    mrange <- getParam "page"
    case mrange of
        Just range -> do
            news <- with db $ getNewsRange $ bs2integer range
            heistLocal (splices news) $ render "/news"
        Nothing    -> redirect "/"
  where
    splices ns = bindSplices [("news", mapSplices renderPost ns)]

handleNewsByYearMonth :: Handler App App ()
handleNewsByYearMonth = do
    myear  <- getParam "year"
    mmonth <- getParam "month"
    case sequence [myear, mmonth] of
        Just [year, month] -> do
            news <- with db $ getNewsByYearMonth (bs2text year)
                (bs2text month)
            heistLocal (splices news) $ render "/news"
        _                  -> redirect "/"
  where
    splices ns = bindSplices [("news", mapSplices renderPost ns)]

handlePostView :: Handler App App ()
handlePostView = do
    mid <- getParam "postid"
    case mid of
        Nothing -> writeBS "error"
        Just lid -> do
            post <- with db $ getPost $ bs2integer lid
            heistLocal (splice post) $ render "/post-view"
  where
    splice p = bindSplices [("post", renderPost p)]
