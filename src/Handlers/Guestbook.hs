{-# LANGUAGE OverloadedStrings #-}

module Handlers.Guestbook
    ( handleGuestbook
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Heist.Interpreted
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Application
import Db
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
  where nodes t = do case X.parseHTML (T.unpack $ gNick g) $ T.encodeUtf8 t of
                      Left err -> return [X.TextNode $ T.pack err]
                      Right d  -> return $ X.docContent d

handleGuestbook :: Handler App App ()
handleGuestbook = do
    guestbook <- with db $ getGuestbook
    heistLocal (splices guestbook) $ render "/guestbook"
  where
    splices gs = bindSplices [("guestbook", mapSplices renderGuestbook gs)]
