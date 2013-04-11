{-# LANGUAGE OverloadedStrings #-}

module Kinds
    ( kindsSplice
    , postsSplice
    ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.SqliteSimple
import qualified Text.XmlHtml as X

import Application
import Db
import Utils

kindsSplice :: Splice (Handler App App)
kindsSplice = 
    return $ map mkoption ["news","guest"]

postsSplice :: Splice (Handler App App)
postsSplice = do
    posts <- lift $ with db getAllPosts
    return $ map mklink (map link navbar) (map name navbar)
