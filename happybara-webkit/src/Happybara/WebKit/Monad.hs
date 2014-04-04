{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, BangPatterns, OverloadedStrings,
             ExistentialQuantification, DeriveDataTypeable #-}

module Happybara.WebKit.Monad where

import Data.Typeable

import Control.Monad.State
import Control.Exception
import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Base
import Control.Monad.Trans.Control

import Happybara.WebKit.Classes
import Happybara.WebKit.Commands as CMD

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

data ElementNotFoundException = forall e . Exception e => ElementNotFoundException e
    deriving Typeable
instance Show ElementNotFoundException where
    show (ElementNotFoundException e) = show e
instance Exception ElementNotFoundException where
    toException = toException . InvalidElementException
    fromException x = do
        InvalidElementException a <- fromException x
        cast a

data AmbiguousElementException = AmbiguousElementException
    deriving (Show, Typeable)
instance Exception AmbiguousElementException where
    toException = toException . ElementNotFoundException
    fromException x = do
        ElementNotFoundException a <- fromException x
        cast a

newtype Browser a = Browser (StateT Session IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)

instance MonadBase IO Browser where
    liftBase = Browser . liftBase

instance MonadBaseControl IO Browser where
    data StM Browser a = StBrowser {unStBrowser :: StM (StateT Session IO) a}

    liftBaseWith f = Browser $
      liftBaseWith $ \runInBase ->
      f (\(Browser sT) -> liftM StBrowser . runInBase $ sT)

    restoreM = Browser . restoreM . unStBrowser

{- instance Driver Browser where -}
    {- data Node Browser = BrowserNode !ByteString -}
