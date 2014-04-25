{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
module Happybara.Node
    ( -- * Scoping
      within
    , withinAll
      -- * Node Manipulation
    , allText
    , visibleText
    , attr
    , getValue
    , setValue
    , selectOption
    , unselectOption
    , click
    , rightClick
    , doubleClick
    , hover
    , dragTo
    , tagName
    , isVisible
    , isChecked
    , isSelected
    , isDisabled
    , path
    , trigger
    , (<==>)
    , (</=>)
    ) where

import           Data.Text           (Text)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Base

import           Happybara.Driver    (Node, NodeValue (..))
import qualified Happybara.Driver    as D
import           Happybara.Monad     as M
import           Happybara.Query

fromQ :: (ToQuery tq sess m) => tq -> HappybaraT sess m (Node sess)
fromQ = findOrFail . toQuery

sync :: HM sess m => IO a -> HappybaraT sess m a
sync = synchronize . liftBase

-- | Set the current element scope to the element given by the query.
within :: (ToQuery tq sess m) => tq -> HappybaraT sess m a -> HappybaraT sess m a
within queryish act = do
    newNode <- findOrFail $ toQuery queryish
    M.withinNode newNode act

-- | For each element given by the query, set the current scope accordingly and
-- invoke the monadic action, yielding each result.
withinAll :: (ToQuery tq sess m) => tq -> HappybaraT sess m a -> HappybaraT sess m [a]
withinAll queryish act = do
    nodes <- findAll $ toQuery queryish
    mapM (flip M.withinNode act) nodes

allText :: (ToQuery tq sess m) => tq -> HappybaraT sess m Text
allText queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.allText driver node

visibleText :: (ToQuery tq sess m) => tq -> HappybaraT sess m Text
visibleText queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.visibleText driver node

attr :: (ToQuery tq sess m) => tq -> Text -> HappybaraT sess m (Maybe Text)
attr queryish name = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.attr driver node name

getValue :: (ToQuery tq sess m) => tq -> HappybaraT sess m NodeValue
getValue queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.getValue driver node

setValue :: (ToQuery tq sess m) => tq -> NodeValue -> HappybaraT sess m ()
setValue queryish val = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.setValue driver node val

selectOption :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
selectOption queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.selectOption driver node

unselectOption :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
unselectOption queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.unselectOption driver node

click :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
click queryish = do
    driver <- getDriver
    node <- fromQ queryish
    sync $ D.click driver node

rightClick :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
rightClick queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.rightClick driver node

doubleClick :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
doubleClick queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.doubleClick driver node

hover :: (ToQuery tq sess m) => tq -> HappybaraT sess m ()
hover queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.hover driver node

dragTo :: (ToQuery tq sess m) => tq -> tq -> HappybaraT sess m ()
dragTo queryish1 queryish2 = do
    node1 <- fromQ queryish1
    node2 <- fromQ queryish2
    driver <- getDriver
    sync $ D.dragTo driver node1 node2

tagName :: (ToQuery tq sess m) => tq -> HappybaraT sess m Text
tagName queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.tagName driver node

isVisible :: (ToQuery tq sess m) => tq -> HappybaraT sess m Bool
isVisible queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.isVisible driver node

isChecked :: (ToQuery tq sess m) => tq -> HappybaraT sess m Bool
isChecked queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.isChecked driver node

isSelected :: (ToQuery tq sess m) => tq -> HappybaraT sess m Bool
isSelected queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.isSelected driver node

isDisabled :: (ToQuery tq sess m) => tq -> HappybaraT sess m Bool
isDisabled queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.isDisabled driver node

path :: (ToQuery tq sess m) => tq -> HappybaraT sess m Text
path queryish = do
    node <- fromQ queryish
    driver <- getDriver
    sync $ D.path driver node

trigger :: (ToQuery tq sess m) => tq -> Text -> HappybaraT sess m ()
trigger queryish event = do
    node <- fromQ queryish
    driver <- getDriver
    liftBase $ D.trigger driver node event

infix 4 <==>
(<==>) :: (ToQuery tq sess m) => tq -> tq -> HappybaraT sess m Bool
queryish1 <==> queryish2 = do
    node1 <- fromQ queryish1
    node2 <- fromQ queryish2
    driver <- getDriver
    sync $ D.nodeEq driver node1 node2

infix 4 </=>
(</=>) :: (ToQuery tq sess m) => tq -> tq -> HappybaraT sess m Bool
queryish1 </=> queryish2 = not <$> (queryish1 <==> queryish2)
