{-# LANGUAGE TypeFamilies               #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
module Happybara.Driver where

import           Data.Aeson
import           Data.Text                   (Text)

import           Network.HTTP.Types          (Header, ResponseHeaders, Status)

import           Happybara.Exceptions
import qualified Happybara.XPath             as X

data NodeValue = SingleValue Text
               | MultiValue [Text]
               deriving (Eq, Show)

data FrameSelector = FrameIndex Int
                   | FrameName Text
                   | DefaultFrame
                   deriving (Eq, Show)

class Driver sess where
    data Node sess :: *
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
