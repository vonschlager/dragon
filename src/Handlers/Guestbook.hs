{-# LANGUAGE OverloadedStrings #-}

module Handlers.Guestbook
    ( handleGuestbook
    , handleGBookByYearMonth
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
import Splices
import Utils

renderGuestbook :: DbGuestbook -> Splice (Handler App App)
renderGuestbook g = runChildrenWith
    [ ("id", textSplice . showAsText $ fromMaybe 0 $ gId g)
    , ("nick", textSplice $ gNick g)
    , ("email", textSplice $ gEmail g)
    , ("www", textSplice $ gWww g)
    , ("body", nodes $ gBody g)
    , ("creation", textSplice . showAsText $ gCreation g)
    ]
  where nodes t = case X.parseHTML (T.unpack $ gNick g) $ T.encodeUtf8 t of
                      Left err -> return [X.TextNode $ T.pack err]
                      Right d  -> return $ X.docContent d

handleGuestbook :: Handler App App ()
handleGuestbook = handleGBookLatest

handleGBookLatest :: Handler App App ()
handleGBookLatest = do
    (year, month) <- with db getGBookLastYearMonth
    guestbook <- with db $ getGBookByYearMonth year month
    heistLocal (splices guestbook (year, month)) $ render "/guestbook"
  where
    splices gs ym = bindSplices [ ("guestbook", mapSplices renderGuestbook gs)
                                , ("sidenav", gbookSideNavSplice ym)
                                ]

handleGBookByYearMonth :: Handler App App ()
handleGBookByYearMonth = do
    myear  <- getParam "year"
    mmonth <- getParam "month"
    case sequence [myear, mmonth] of
        Just [year, month] -> do
            guestbook <- with db $ getGBookByYearMonth (bs2t year)
                (bs2t month)
            heistLocal (splices guestbook (bs2t year, bs2t month)) $
                render "/guestbook"
        _                  -> redirect "/"
  where
    splices gs ym = bindSplices [ ("guestbook", mapSplices renderGuestbook gs)
                                , ("sidenav", gbookSideNavSplice ym)
                                ]
