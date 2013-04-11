{-# LANGUAGE OverloadedStrings #-}

module Handlers.Navbar
    ( handleNavbar
    , handleNavbarAdd
    , handleNavbarAddKind
    , handleNavbarAddPost
    , handleNavbarAddOther
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
    mapSplices renderLi navbar
  where
    renderLi :: Navbar -> Splice (Handler App App)
    renderLi n = runChildrenWithText
        [ ("link", link n)
        , ("name", name n)
        ]

renderNavbar :: Navbar -> Splice (Handler App App)
renderNavbar n = runChildrenWithText
    [ ("entryid", showAsText $ fromMaybe 0 $ entryid n)
    , ("name", name n)
    , ("link", link n)
    ]

handleNavbar :: Handler App App ()
handleNavbar = do
    navbar <- with db getNavbar
    heistLocal (splices navbar) $ render "/adminnavbar"
  where
    splices ne = bindSplices [("adminnavbar", mapSplices renderNavbar ne)]

handleNavbarAddKind :: Handler App App ()
handleNavbarAddKind = method GET $ render "/navbaraddkindform"

handleNavbarAddPost :: Handler App App ()
handleNavbarAddPost = method GET $ render "/navbaraddpostform"

handleNavbarAddOther :: Handler App App ()
handleNavbarAddOther = method GET $ render "/navbaraddotherform"

handleNavbarAdd :: Handler App App ()
handleNavbarAdd =
    method POST handleNavbarSubmit
  where
    handleNavbarSubmit = do
        mname <- getPostParam "name"
        mlink <- getPostParam "link"
        morder <- getPostParam "order"
        case sequence [mname, mlink, morder] of
            Nothing -> writeBS "error"
            (Just [_name, _link, _order]) -> do
                let navbar = Navbar Nothing
                                    (bs2text _name)
                                    (bs2text _link)
                                    (bs2integer _order)
                with db $ saveNavbar navbar
                redirect "/admin/navbar"

handleNavbarEdit :: Handler App App ()
handleNavbarEdit = undefined

handleNavbarDelete :: Handler App App ()
handleNavbarDelete = do
    mid <- getParam "entryid"
    case mid of
        Nothing -> writeBS "error"
        Just _id -> do
            with db $ deleteNavbar $ bs2integer _id
            redirect "/admin/navbar"

