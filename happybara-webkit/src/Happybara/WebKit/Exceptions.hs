{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

module Happybara.WebKit.Exceptions where

import           Control.Exception
import           Data.Typeable

import           Happybara.Exceptions

data NodeNotAttachedException = NodeNotAttachedException String
    deriving (Show, Typeable)
instance Exception NodeNotAttachedException where
    toException = toException . InvalidElementException
    fromException x = do
        InvalidElementException a <- fromException x
        cast a

data TimeoutException = TimeoutException String
    deriving (Show, Typeable)
instance Exception TimeoutException where
    toException = toException . HappybaraException
    fromException x = do
        HappybaraException a <- fromException x
        cast a

data InvalidResponseException = InvalidResponseException String
    deriving (Show, Typeable)
instance Exception InvalidResponseException where
    toException = toException . HappybaraException
    fromException x = do
        HappybaraException a <- fromException x
        cast a

data NoResponseException = NoResponseException String
    deriving (Show, Typeable)
instance Exception NoResponseException where
    toException = toException . HappybaraException
    fromException x = do
        HappybaraException a <- fromException x
        cast a

data ClickFailedException = ClickFailedException String
    deriving (Show, Typeable)
instance Exception ClickFailedException where
    toException = toException . HappybaraException
    fromException x = do
        HappybaraException a <- fromException x
        cast a
