{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Happybara.Query
    ( Query
    , SimpleQuery(..)
    , find
    , findOrFail
    , findAll
      -- scoping
    , within
    , withinAll
      -- basic queries
    , link
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
      -- predicates
    , href
    , checked
    , unchecked
    , disabled
    , selected
    , options
    , elemType
    ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control

import           Data.List                   (sort)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Happybara.Driver            (Driver, Node, NodeValue (..))
import qualified Happybara.Driver            as D
import           Happybara.Exceptions
import           Happybara.Monad
import qualified Happybara.Monad             as M
import qualified Happybara.XPath             as X

class (Driver sess, MonadIO m, MonadBase IO m, MonadBaseControl IO m) => Query q sess m where
    find             :: q sess m -> HappybaraT sess m (Maybe (Node sess))
    findOrFail       :: q sess m -> HappybaraT sess m (Node sess)
    findAll          :: q sess m -> HappybaraT sess m [Node sess]

data SimpleQuery sess m = SimpleQuery { sqXPath       :: (Bool -> Text)
                                      , sqPredicates  :: [(Node sess) -> HappybaraT sess m Bool]
                                      , sqDescription :: Text
                                      }

instance (Driver sess, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
      => Query SimpleQuery sess m where
    find q = do
        (Just <$> findOrFail q) `catch` (\(e :: InvalidElementException) ->
            return $ Nothing)

    findOrFail q = do
        M.synchronize $ do
            matchStrategy <- M.getSingleMatchStrategy
            results <- findAll q
            when (null results) $ do
              liftBase $ throw ElementNotFoundException
            case matchStrategy of
                MatchFirst -> return $ head results
                MatchOne -> do
                    if isAmbiguous results
                      then liftBase $ throw AmbiguousElementException
                      else return $ head results
      where
        isAmbiguous (n1:n2:_) = True
        isAmbiguous _         = False

    findAll (SimpleQuery xpath preds _) = do
        exactness <- M.getExactness
        case exactness of
            Exact -> do
                find $ xpath True
            PreferExact -> do
                res <- find $ xpath True
                if null res
                  then find $ xpath False
                  else return res
            Inexact -> do
                find $ xpath False
      where
        allM _ []     = return True
        allM f (b:bs) = (f b) >>= (\bv -> if bv then allM f bs else return False)
        compositePredicate n = allM (\p -> p n) preds
        find x = do
            res <- M.findXPath x
            filterM compositePredicate res

-- scoping

within :: (Query q sess m, Driver sess, Functor m, Monad m)
       => q sess m -> HappybaraT sess m a -> HappybaraT sess m a
within query act = do
    newNode <- findOrFail query
    M.withinNode newNode act

withinAll :: (Query q sess m, Driver sess, Functor m, Monad m)
          => q sess m -> HappybaraT sess m a -> HappybaraT sess m [a]
withinAll query act = do
    nodes <- findAll query
    mapM (flip M.withinNode act) nodes

-- basic queries

mkQuery :: (Driver sess) => Text -> Text -> (Text -> Bool -> Text) -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
mkQuery ty locator xpath preds =
    SimpleQuery (xpath locator) preds (locatorDescription ty locator)
  where
    escapeText = T.pack . show
    locatorDescription ty locator =
        T.concat [ty, ": ", escapeText locator]

link :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
link locator preds =
    mkQuery "link" locator X.link preds

button :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
button locator preds =
    mkQuery "button" locator X.button preds

linkOrButton :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
linkOrButton locator preds =
    mkQuery "linkOrButton" locator X.linkOrButton preds

fieldset :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
fieldset locator preds =
    mkQuery "fieldset" locator X.fieldset preds

field :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
field locator preds =
    mkQuery "field" locator X.field preds

fillableField :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
fillableField locator preds =
    mkQuery "fillableField" locator X.fillableField preds

select :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
select locator preds =
    mkQuery "select" locator X.select preds

checkbox :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
checkbox locator preds =
    mkQuery "checkbox" locator X.checkbox preds

radioButton :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
radioButton locator preds =
    mkQuery "radioButton" locator X.radioButton preds

fileField :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
fileField locator preds =
    mkQuery "fileField" locator X.fileField preds

optgroup :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
optgroup locator preds =
    mkQuery "optgroup" locator X.optgroup preds

option :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
option locator preds =
    mkQuery "option" locator X.option preds

table :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
table locator preds =
    mkQuery "table" locator X.table preds

definitionDescription :: (Driver sess) => Text -> [Node sess -> HappybaraT sess m Bool] -> SimpleQuery sess m
definitionDescription locator preds =
    mkQuery "definitionDescription" locator (const . X.definitionDescription) preds

-- predicates

href :: (Driver sess, MonadBase IO m) => Text -> Node sess -> HappybaraT sess m Bool
href url node = do
    driver <- M.getDriver
    liftBase $ do
        (not . null) <$> D.findXPathRel driver node xpath
  where
    xpath = T.concat ["./self::*[./@href = ", X.stringLiteral url, "]"]

checked :: (Driver sess, MonadBase IO m) => Bool -> Node sess -> HappybaraT sess m Bool
checked b node = do
    driver <- M.getDriver
    liftBase $ do
        (b==) <$> D.isChecked driver node

unchecked :: (Driver sess, MonadBase IO m) => Bool -> Node sess -> HappybaraT sess m Bool
unchecked b node = do
    driver <- M.getDriver
    liftBase $ do
        (b/=) <$> D.isChecked driver node

disabled :: (Driver sess, MonadBase IO m) => Bool -> Node sess -> HappybaraT sess m Bool
disabled b node = do
    driver <- M.getDriver
    liftBase $ do
        name <- D.tagName driver node
        if name == "a"
          then return True
          else (b==) <$> D.isDisabled driver node

selected :: (Driver sess, MonadBase IO m) => NodeValue -> Node sess -> HappybaraT sess m Bool
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

options :: (Driver sess, MonadBase IO m) => [Text] -> Node sess -> HappybaraT sess m Bool
options opts node = do
    driver <- M.getDriver
    liftBase $ do
        options <- D.findXPathRel driver node ".//option"
        actual <- mapM (D.visibleText driver) options
        return $ (sort opts) == (sort actual)

elemType :: (Driver sess, MonadBase IO m) => Text -> Node sess -> HappybaraT sess m Bool
elemType t node = do
    driver <- M.getDriver
    liftBase $ do
        if any (t==) ["textarea", "select"]
          then (t==) <$> D.tagName driver node
          else (Just t==) <$> D.attr driver node "type"
