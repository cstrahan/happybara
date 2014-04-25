{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
module Happybara.Queries (
      -- * Basic Queries
      -- $queries
      link
    , button
    , linkOrButton
    , fieldset
    , field
    , fillableField
    , select
    , checkbox
    , radioButton
    , fileField
    , optgroup
    , option
    , table
    , definitionDescription
      -- * Predicates
    , href
    , checked
    , unchecked
    , disabled
    , selected
    , options
    , elemType
    ) where

import           Data.List           (sort)
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Base

import           Happybara.Driver    (Node, NodeValue (..))
import qualified Happybara.Driver    as D
import           Happybara.Monad     as M
import           Happybara.Query
import qualified Happybara.XPath     as X

mkXPathQuery :: (HM sess m) => Text -> Text -> (Text -> Bool -> Text) -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
mkXPathQuery ty locator xpathGen predicates =
    mkQuery $ do
        exactness <- M.getExactness
        case exactness of
            Exact -> do
                find $ xpathGen locator True
            PreferExact -> do
                res <- find $ xpathGen locator True
                if null res
                  then find $ xpathGen locator False
                  else return res
            Inexact -> do
                find $ xpathGen locator False
  where
    find xpath =
        M.findXPath xpath >>= filterM (composePredicates predicates)

-- $queries
-- Happybara includes a number of queries for common cases where you want to
-- find an element by value, title, id, alt-text, etc.
-- These queries can be further filtered by the predicates listed below.
--
-- For example, this query will return all enabled buttons matching \"Submit Application\":
--
-- @
--'findAll' $ 'button' \"Submit Application\" [ 'disabled' False ]
-- @

link :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
link locator preds =
    mkXPathQuery "link" locator X.link preds

button :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
button locator preds =
    mkXPathQuery "button" locator X.button preds

linkOrButton :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
linkOrButton locator preds =
    mkXPathQuery "linkOrButton" locator X.linkOrButton preds

fieldset :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
fieldset locator preds =
    mkXPathQuery "fieldset" locator X.fieldset preds

field :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
field locator preds =
    mkXPathQuery "field" locator X.field preds

fillableField :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
fillableField locator preds =
    mkXPathQuery "fillableField" locator X.fillableField preds

select :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
select locator preds =
    mkXPathQuery "select" locator X.select preds

checkbox :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
checkbox locator preds =
    mkXPathQuery "checkbox" locator X.checkbox preds

radioButton :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
radioButton locator preds =
    mkXPathQuery "radioButton" locator X.radioButton preds

fileField :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
fileField locator preds =
    mkXPathQuery "fileField" locator X.fileField preds

optgroup :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
optgroup locator preds =
    mkXPathQuery "optgroup" locator X.optgroup preds

option :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
option locator preds =
    mkXPathQuery "option" locator X.option preds

table :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
table locator preds =
    mkXPathQuery "table" locator X.table preds

definitionDescription :: (HM sess m) => Text -> [Node sess -> HappybaraT sess m Bool] -> Query sess m
definitionDescription locator preds =
    mkXPathQuery "definitionDescription" locator (const . X.definitionDescription) preds

-- predicates

href :: (HM sess m) => Text -> Node sess -> HappybaraT sess m Bool
href url node = do
    driver <- M.getDriver
    liftBase $ do
        (not . null) <$> D.findXPathRel driver node xpath
  where
    xpath = T.concat ["./self::*[./@href = ", X.stringLiteral url, "]"]

checked :: (HM sess m) => Bool -> Node sess -> HappybaraT sess m Bool
checked b node = do
    driver <- M.getDriver
    liftBase $ do
        (b==) <$> D.isChecked driver node

unchecked :: (HM sess m) => Bool -> Node sess -> HappybaraT sess m Bool
unchecked b node = do
    driver <- M.getDriver
    liftBase $ do
        (b/=) <$> D.isChecked driver node

disabled :: (HM sess m) => Bool -> Node sess -> HappybaraT sess m Bool
disabled b node = do
    driver <- M.getDriver
    liftBase $ do
        name <- D.tagName driver node
        if name == "a"
          then return True
          else (b==) <$> D.isDisabled driver node

selected :: (HM sess m) => NodeValue -> Node sess -> HappybaraT sess m Bool
selected (SingleValue val) node = do
    driver <- M.getDriver
    liftBase $ do
        ((SingleValue val) ==) <$> D.getValue driver node
selected (MultiValue vals) node = do
    driver <- M.getDriver
    liftBase $ do
        opts <- D.findXPathRel driver node ".//option"
        seld <- filterM (D.isSelected driver) opts
        texts <- mapM (D.visibleText driver) seld
        return $ sort vals == sort texts

options :: (HM sess m) => [Text] -> Node sess -> HappybaraT sess m Bool
options opts node = do
    driver <- M.getDriver
    liftBase $ do
        options <- D.findXPathRel driver node ".//option"
        actual <- mapM (D.visibleText driver) options
        return $ (sort opts) == (sort actual)

elemType :: (HM sess m) => Text -> Node sess -> HappybaraT sess m Bool
elemType t node = do
    driver <- M.getDriver
    liftBase $ do
        if any (t==) ["textarea", "select"]
          then (t==) <$> D.tagName driver node
          else (Just t==) <$> D.attr driver node "type"
