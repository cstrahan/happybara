{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
module Happybara.Query
    ( -- * Query Interface
      Query(..)
    , ToQuery(..)
    , mkQuery
    , composePredicates
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

-- | This class is the backbone of Happybara's DOM querying DSL.
-- While Happybara includes support for a number of common queries, you're more than
-- welcome to implement your own 'Query' instances, thus extending the DSL.
--
-- Queries are scoped to the current node as given by 'M.getCurrentNode', and
-- a new scope can be specified via 'Happybara.Node.within'.
--
-- Note that the behavior of a query is dependent on the current 'M.Exactness' setting:
--
-- * 'M.Exact' - Find elements that match exactly.
--
-- * 'M.PreferExact' - First try to find exact matches; if that fails, fall
-- back to inexact matches.
--
-- * 'M.Inexact' - Find all elements that partially match - e.g. the given
-- string is infix of (but not necessarily equal to) whatever property (id,
-- attribute, etc) is being queried over.
--
-- When locating a single item, the failure mode depends on the current 'SingleMatchStrategy' setting:
--
-- * 'MatchFirst' - If no elements matched, throw 'ElementNotFoundException';
-- otherwise, return the first matching element.
--
-- * 'MatchOne' - If no elements matched, throw 'ElementNotFoundException'; if
-- more than element matches, throw 'AmbiguousElementException'.
--
-- To set the current 'M.Exactness', use 'M.setExactness'.
-- To set the current 'M.SingleMatchStrategy', use 'M.setSingleMatchStrategy'.
data Query sess m = Query { find       :: HappybaraT sess m (Maybe (Node sess))
                          , findOrFail :: HappybaraT sess m (Node sess)
                          , findAll    :: HappybaraT sess m [Node sess]
                          }

class (HM sess m) => ToQuery tq sess m | tq -> sess, tq -> m where
    toQuery :: tq -> Query sess m

instance (HM sess m) => ToQuery (Node sess) sess m where
    toQuery = mkQuery . return . (:[])

instance (HM sess m) => ToQuery (Maybe (Node sess)) sess m where
    toQuery = mkQuery . return . maybe [] (:[])

instance (HM sess m) => ToQuery [Node sess] sess m where
    toQuery = mkQuery . return

instance (HM sess m) => ToQuery (HappybaraT sess m (Node sess)) sess m where
    toQuery = mkQuery . fmap (:[])

instance (HM sess m) => ToQuery (HappybaraT sess m (Maybe (Node sess))) sess m where
    toQuery = mkQuery . fmap (maybe [] (:[]))

instance (HM sess m) => ToQuery (HappybaraT sess m [Node sess]) sess m where
    toQuery = mkQuery

instance (HM sess m) => ToQuery (Query sess m) sess m where
    toQuery = id

composePredicates :: (HM sess m) => [Node sess -> HappybaraT sess m Bool] -> (Node sess -> HappybaraT sess m Bool)
composePredicates predicates node =
    allM ($ node) predicates
  where
    allM _ []     = return True
    allM f (b:bs) = (f b) >>= (\bv -> if bv then allM f bs else return False)

mkQuery :: (HM sess m) => HappybaraT sess m [Node sess] -> Query sess m
mkQuery all =
    Query { find = find', findOrFail = findOrFail', findAll = all }
  where
    find' = do
        (Just <$> findOrFail') `catch` (\(e :: InvalidElementException) ->
            return $ Nothing)

    findOrFail' = do
        M.synchronize $ do
            matchStrategy <- M.getSingleMatchStrategy
            results <- all
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
