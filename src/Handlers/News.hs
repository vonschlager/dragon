{-# LANGUAGE OverloadedStrings #-}

module Handlers.News
    ( handleNews
    , handleNewsByYearMonth
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Application
import Db
import Splices
import Utils

renderPost :: DbPost -> Splice (Handler App App)
renderPost p = runChildrenWith
    [ ("title", textSplice $ pTitle p)
    , ("body", nodes $ pBody p)
    , ("creation", textSplice . showAsText $ pCreation p)
    , ("publish", textSplice . showAsText $ pPublish p)
    ]
  where nodes t =  case X.parseHTML (T.unpack $ pTitle p) $ T.encodeUtf8 t of
                      Left err -> return [X.TextNode $ T.pack err]
                      Right d  -> return $ X.docContent d

handleNews :: Handler App App ()
handleNews = handleNewsLatest

handleNewsLatest :: Handler App App ()
handleNewsLatest = do
    (year, month) <- with db getNewsLastYearMonth
    news <- with db $ getNewsByYearMonth year month
    heistLocal (splices news (year, month)) $ render "/news"
  where
    splices ns ym = bindSplices [ ("news", mapSplices renderPost ns)
                                , ("sidenav", sidenavSplice ym)
                                ]

handleNewsByYearMonth :: Handler App App ()
handleNewsByYearMonth = do
    myear  <- getParam "year"
    mmonth <- getParam "month"
    case sequence [myear, mmonth] of
        Just [year, month] -> do
            news <- with db $ getNewsByYearMonth (bs2text year)
                (bs2text month)
            heistLocal (splices news (bs2text year, bs2text month)) $
                render "/news"
        _                  -> redirect "/"
  where
    splices ns ym = bindSplices [ ("news", mapSplices renderPost ns)
                                , ("sidenav", sidenavSplice ym)
                                ]
