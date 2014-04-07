{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Happybara.Classes where

import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (isDigit)
import           Data.List                   (isPrefixOf, sort)
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Typeable
import qualified Data.Word8                  as BS

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Control

import           System.FilePath
import           System.IO
import           System.Process
import           System.Timeout

import           Network.BSD                 as Net
import           Network.HTTP.Types          (Header, ResponseHeaders, Status)
import qualified Network.Socket              as Net

import           System.Info                 (os)

import           Happybara.Exceptions
import           Happybara.WebKit.Exceptions
import qualified Happybara.XPath             as X
import           Paths_happybara_webkit      (getDataFileName, getLibexecDir)

data NodeValue = SingleValue Text
               | MultiValue [Text]
               deriving (Eq, Ord)

data FrameSelector = FrameIndex Int
                   | FrameName Text
                   | DefaultFrame

data Exactness = Exact
               | PreferExact
               | Inexact

class Query q where
    type QueryDriver q :: *

    queryDescription :: q -> String

    withExactness    :: q -> Exactness -> q

    relativeTo'      :: q -> Maybe (Node (QueryDriver q)) -> q

    relativeTo       :: q -> Node (QueryDriver q) -> q
    relativeTo q n =
        relativeTo' q (Just n)

    findAll          :: (QueryDriver q) -> q -> IO [Node (QueryDriver q)]

    findFirst        :: (QueryDriver q) -> q -> IO (Maybe (Node (QueryDriver q)))
    findFirst sess q =
        findAll sess q >>= go
      where
        go (node:_) = return $ Just node
        go []       = return $ Nothing

    findFirstOrFail  :: (Query q) => (QueryDriver q) -> q -> IO (Node (QueryDriver q))
    findFirstOrFail sess q = do
        mnode <- findFirst sess q
        case mnode of
            Just mnode -> return mnode
            _          -> throw ElementNotFoundException

    findOneOrFail    :: (Query q) => (QueryDriver q) -> q -> IO (Node (QueryDriver q))
    findOneOrFail sess q =
        findAll sess q >>= go
      where
        go (node:[]) = return node
        go (node:_)  = throw AmbiguousElementException
        go []        = throw ElementNotFoundException

class Driver sess where
    data Node sess :: *
    -- | @waitNested@ is
    waitNested      :: sess -> Double -> IO a -> IO a
    currentUrl      :: sess -> IO Text
    visit           :: sess -> Text -> IO ()
    findXPath       :: sess -> Text -> IO [Node sess]
    findCSS         :: sess -> Text -> IO [Node sess]
    html            :: sess -> IO Text
    goBack          :: sess -> IO ()
    goForward       :: sess -> IO ()
    executeScript   :: sess -> Text -> IO ()
    evaluateScript  :: sess -> Text -> IO Value
    saveScreenshot  :: sess -> Text -> Int -> Int -> IO ()
    responseHeaders :: sess -> IO ResponseHeaders
    statusCode      :: sess -> IO Status
    withinFrame     :: sess -> FrameSelector -> IO a -> IO a
    withinWindow    :: sess -> Text -> IO a -> IO a
    reset           :: sess -> IO ()
    findXPathRel    :: sess -> Node sess -> Text -> IO [Node sess]
    findCSSRel      :: sess -> Node sess -> Text -> IO [Node sess]
    allText         :: sess -> Node sess -> IO Text
    visibleText     :: sess -> Node sess -> IO Text
    attr            :: sess -> Node sess -> Text -> IO (Maybe Text)
    getValue        :: sess -> Node sess -> IO NodeValue
    setValue        :: sess -> Node sess -> NodeValue -> IO ()
    selectOption    :: sess -> Node sess -> IO ()
    unselectOption  :: sess -> Node sess -> IO ()
    click           :: sess -> Node sess -> IO ()
    rightClick      :: sess -> Node sess -> IO ()
    doubleClick     :: sess -> Node sess -> IO ()
    hover           :: sess -> Node sess -> IO ()
    dragTo          :: sess -> Node sess -> Node sess -> IO ()
    tagName         :: sess -> Node sess -> IO Text
    isVisible       :: sess -> Node sess -> IO Bool
    isChecked       :: sess -> Node sess -> IO Bool
    isSelected      :: sess -> Node sess -> IO Bool
    isDisabled      :: sess -> Node sess -> IO Bool
    path            :: sess -> Node sess -> IO Text
    trigger         :: sess -> Node sess -> Text -> IO ()
    nodeEq          :: sess -> Node sess -> Node sess -> IO Bool -- infix 4 <==>
