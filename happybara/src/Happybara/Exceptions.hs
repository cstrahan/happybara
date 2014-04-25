{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
--
-- Happybara uses the following heirarchy of exceptions to communicate failures:
--
-- @
-- 'HappybaraException'
-- └── 'InvalidElementException'
--     ├── 'ExpectationNotMetException'
--     ├── 'ElementNotFoundException'
--     └── 'AmbiguousElementException'
-- @
--
-- To handle the asynchronicity inherent in sites that use JavaScript, Happybara
-- will retry your requests. Behind the scenes, 'Happybara.Monad.synchronize'
-- will catch any exception of type 'InvalidElementException' (or subclass
-- thereof) and retry according to the associated docs.
module Happybara.Exceptions where

import           Control.Exception
import           Data.Typeable

data HappybaraException = forall e . Exception e => HappybaraException e
    deriving Typeable
instance Show HappybaraException where
    show (HappybaraException e) = show e
instance Exception HappybaraException

data InvalidElementException = forall e . Exception e => InvalidElementException e
    deriving Typeable
instance Show InvalidElementException where
    show (InvalidElementException e) = show e
instance Exception InvalidElementException where
    toException = toException . HappybaraException
    fromException x = do
        HappybaraException a <- fromException x
        cast a

data ExpectationNotMetException = ExpectationNotMetException
    deriving (Show, Typeable)
instance Exception ExpectationNotMetException where
    toException = toException . InvalidElementException
    fromException x = do
        InvalidElementException a <- fromException x
        cast a

data ElementNotFoundException = ElementNotFoundException
    deriving (Show, Typeable)
instance Exception ElementNotFoundException where
    toException = toException . InvalidElementException
    fromException x = do
        InvalidElementException a <- fromException x
        cast a

data AmbiguousElementException = AmbiguousElementException
    deriving (Show, Typeable)
instance Exception AmbiguousElementException where
    toException = toException . InvalidElementException
    fromException x = do
        InvalidElementException a <- fromException x
        cast a
