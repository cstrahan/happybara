{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Happybara.Query where

import           Control.Applicative
import           Control.Monad

import           Data.List
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Happybara.Classes
import qualified Happybara.XPath     as X

-- instances

data SimpleQuery sess where
    MkSimpleQuery :: (Driver sess)
                  => Exactness
                  -> (Bool -> Text)
                  -> (Maybe (Node sess))
                  -> [sess -> (Node sess) -> IO Bool]
                  -> Text
                  -> SimpleQuery sess

instance Query (SimpleQuery sess) where
    type QueryDriver (SimpleQuery sess) = sess

    queryDescription (MkSimpleQuery _ _ _ _ desc) =
        T.unpack desc

    relativeTo' (MkSimpleQuery a b _ c d) rel =
        MkSimpleQuery a b rel c d

    withExactness (MkSimpleQuery _ a b c d) exact =
        MkSimpleQuery exact a b c d

    findAll sess (MkSimpleQuery exact xpath mrel preds _) =
        case exact of
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
        pred x = allM (\p -> p sess x) preds
        find x = do
            res <- case mrel of
                       Just node -> findXPathRel sess node x
                       _         -> findXPath sess x
            filterM pred res

-- exactness in matching

exact :: (Query q) => q -> q
exact = flip withExactness Exact

preferExact :: (Query q) => q -> q
preferExact = flip withExactness PreferExact

inexact :: (Query q) => q -> q
inexact = flip withExactness Inexact

-- basic queries

link :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
link locator preds =
    MkSimpleQuery PreferExact (X.link locator) Nothing preds "link"

button :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
button locator preds =
    MkSimpleQuery PreferExact (X.button locator) Nothing preds "button"

linkOrButton :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
linkOrButton locator preds =
    MkSimpleQuery PreferExact (X.linkOrButton locator) Nothing preds "linkOrButton"

fieldset :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
fieldset locator preds =
    MkSimpleQuery PreferExact (X.fieldset locator) Nothing preds "fieldset"

field :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
field locator preds =
    MkSimpleQuery PreferExact (X.field locator) Nothing preds "field"

fillableField :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
fillableField locator preds =
    MkSimpleQuery PreferExact (X.fillableField locator) Nothing preds "fillableField"

select :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
select locator preds =
    MkSimpleQuery PreferExact (X.select locator) Nothing preds "select"

checkbox :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
checkbox locator preds =
    MkSimpleQuery PreferExact (X.checkbox locator) Nothing preds "checkbox"

radioButton :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
radioButton locator preds =
    MkSimpleQuery PreferExact (X.radioButton locator) Nothing preds "radioButton"

fileField :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
fileField locator preds =
    MkSimpleQuery PreferExact (X.fileField locator) Nothing preds "fileField"

optgroup :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
optgroup locator preds =
    MkSimpleQuery PreferExact (X.optgroup locator) Nothing preds "optgroup"

option :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
option locator preds =
    MkSimpleQuery PreferExact (X.option locator) Nothing preds "option"

table :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
table locator preds =
    MkSimpleQuery PreferExact (X.table locator) Nothing preds "table"

definitionDescription :: (Driver sess) => Text -> [sess -> Node sess -> IO Bool] -> SimpleQuery sess
definitionDescription locator preds =
    MkSimpleQuery PreferExact (const $ X.definitionDescription locator) Nothing preds "definitionDescription"

-- predicates

href :: (Driver sess) => Text -> sess -> Node sess -> IO Bool
href url sess node =
    (not . null) <$> findXPathRel sess node xpath
  where
    xpath = T.concat ["./self::*[./@href = ", X.stringLiteral url, "]"]

checked :: (Driver sess) => Bool -> sess -> Node sess -> IO Bool
checked b sess node =
    (b==) <$> isChecked sess node

unchecked :: (Driver sess) => Bool -> sess -> Node sess -> IO Bool
unchecked b sess node =
    (b/=) <$> isChecked sess node

disabled :: (Driver sess) => Bool -> sess -> Node sess -> IO Bool
disabled b sess node = do
    name <- tagName sess node
    if name == "a"
      then return True
      else (b==) <$> isDisabled sess node

selected :: (Driver sess) => NodeValue -> sess -> Node sess -> IO Bool
selected (SingleValue val) sess node =
    ((SingleValue val) ==) <$> getValue sess node
selected (MultiValue vals) sess node = do
    opts <- findXPathRel sess node ".//option"
    seld <- filterM (isSelected sess) opts
    texts <- mapM (visibleText sess) seld
    return $ sort vals == sort texts

options :: (Driver sess) => [Text] -> sess -> Node sess -> IO Bool
options opts sess node = do
    options <- findXPathRel sess node ".//option"
    actual <- mapM (visibleText sess) options
    return $ (sort opts) == (sort actual)

elemType :: (Driver sess) => Text -> sess -> Node sess -> IO Bool
elemType t sess node =
    if any (t==) ["textarea", "select"]
      then (t==) <$> tagName sess node
      else (Just t==) <$> attr sess node "type"
