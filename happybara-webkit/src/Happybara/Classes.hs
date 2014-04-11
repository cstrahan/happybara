{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.State

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
import           Paths_happybara_webkit      (getDataFileName)

data NodeValue = SingleValue Text
               | MultiValue [Text]
               deriving (Eq, Show)

data FrameSelector = FrameIndex Int
                   | FrameName Text
                   | DefaultFrame
                   deriving (Eq, Show)

data Exactness = Exact
               | PreferExact
               | Inexact
               deriving (Eq, Ord, Show)

data SingleMatchStrategy = MatchOne
                         | MatchFirst

data HappybaraState sess = HappybaraState { driver              :: sess
                                          , wait                :: Double
                                          , exactness           :: Exactness
                                          , isSynced            :: Bool
                                          , singleMatchStrategy :: SingleMatchStrategy
                                          , currentNode         :: Maybe (Node sess)
                                          }

type Happybara sess a = HappybaraT sess IO a
newtype HappybaraT sess m a = HappybaraT { unHappybaraT :: StateT (HappybaraState sess) m a }
                              deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class (Driver sess, MonadIO m, MonadBase IO m, MonadBaseControl IO m) => Query q sess m where
    queryDescription :: q sess m -> String
    find             :: q sess m -> HappybaraT sess m (Maybe (Node sess))
    findOrFail       :: q sess m -> HappybaraT sess m (Node sess)
    findAll          :: q sess m -> HappybaraT sess m [Node sess]

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
    setFrameFocus   :: sess -> FrameSelector -> IO ()
    setWindowFocus  :: sess -> Text -> IO ()
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
