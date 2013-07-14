{-# LANGUAGE OverloadedStrings #-}

module Handlers.Match
    ( handleMatch
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

renderPost :: DbPost -> Splice (Handler App App)
renderPost p = runChildrenWith
    [ ("title", textSplice $ pTitle p)
    , ("body", nodes $ pBody p)
    ]
  where nodes t = case X.parseHTML (T.unpack $ pTitle p) $ T.encodeUtf8 t of
                      Left err -> return [X.TextNode $ T.pack err]
                      Right d  -> return $ X.docContent d

handleMatch :: Handler App App ()
handleMatch = do
    match <- with db $ getMatch
    heistLocal (splices match) $ render "/match"
  where
    splices ps = bindSplices [ ("match", mapSplices renderPost ps)
                           --, ("sidenav", siderNavSplice ym)
                             ]
