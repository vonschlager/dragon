{-# LANGUAGE OverloadedStrings #-}

module Handlers.Navbar
    ( handleNavbar
    , handleNavbarAdd
    , handleNavbarEdit
    , handleNavbarDelete
    , navbarSplice
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

navbarSplice :: Splice (Handler App App)
navbarSplice = do
    navbar <- lift $ with db getNavbar
    return $ zipWith mklink (map link navbar) (map name navbar)

renderNavbar :: Navbar -> Splice (Handler App App)
renderNavbar n = runChildrenWithText
    [ ("name", name n)
    , ("link", link n)
    ]

handleNavbar :: Handler App App ()
handleNavbar = do
    navbar <- with db getNavbar
    heistLocal (splices navbar) $ render "/adminnavbar"
  where
    splices ne = bindSplices [("adminnavbar", mapSplices renderNavbar ne)]

handleNavbarAdd :: Handler App App ()
handleNavbarAdd =
    method GET renderNavbarAddForm <|> method POST handleNavbarSubmit
  where
    renderNavbarAddForm = render "/navbaraddform"      
    handleNavbarSubmit = do
        mname <- getPostParam "name"
        mkind <- getPostParam "kind"
        case sequence [mname, mkind] of
            Nothing -> writeBS "error"
            (Just [lname, lkind]) -> do
                let navbar = Navbar Nothing
                                    (bs2text lname)
                                    (bs2text lkind)
                                    0
                with db $ saveNavbar navbar
                redirect "/posts"

handleNavbarEdit :: Handler App App ()
handleNavbarEdit = undefined

handleNavbarDelete :: Handler App App ()
handleNavbarDelete = do
    mid <- getParam "entryid"
    case mid of
        Nothing -> writeBS "error"
        Just lid -> do
            with db $ deleteNavbar $ bs2integer lid
            redirect "/posts"

