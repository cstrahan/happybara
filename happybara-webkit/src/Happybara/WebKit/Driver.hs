{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Happybara.WebKit.Driver where

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)

import           Network.HTTP.Types

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Base
import           Control.Monad.State
import           Control.Monad.Trans.Control

import           Happybara.Driver
import           Happybara.WebKit.Session
import           Happybara.WebKit.Commands   as CMD

instance Driver Session where
    data Node Session = WebKitNode !Text deriving (Show)
    currentUrl sess = do
        CMD.currentUrl sess
    visit sess url = CMD.visit sess url
    findXPath sess query = do
        handles <- CMD.findXPath sess query
        return $ map WebKitNode handles
    findCSS sess query = do
        handles <- CMD.findCSS sess query
        return $ map WebKitNode handles
    html sess = do
        CMD.body sess
    goBack = error "NOT IMPLEMENTED"
    goForward = error "NOT IMPLEMENTED"
    executeScript sess script = do
        CMD.executeScript sess script
    evaluateScript sess script = do
        CMD.evaluateScript sess script
    saveScreenshot sess path width height = do
        CMD.render sess path width height
    responseHeaders sess = do
        CMD.responseHeaders sess
    statusCode sess = do
        toEnum <$> CMD.statusCode sess
    setFrameFocus sess frameId = do
        CMD.setFrameFocus sess frameId
    setWindowFocus sess name = error "NOT IMPLEMENTED"
    reset sess = do
        CMD.reset sess
    findXPathRel sess (WebKitNode h) query = do
        handles <- CMD.findXPathRel sess h query
        return $ map WebKitNode handles
    findCSSRel sess (WebKitNode h) query = do
        handles <- CMD.findCSSRel sess h query
        return $ map WebKitNode handles
    allText sess (WebKitNode h) = do
        CMD.allText sess h
    visibleText sess (WebKitNode h) = do
        CMD.visibleText sess h
    attr sess (WebKitNode h) name = do
        CMD.attr sess h name
    getValue sess (WebKitNode h) = do
        CMD.value sess h
    setValue sess (WebKitNode h) val = do
        CMD.set sess h val
    selectOption sess (WebKitNode h) = do
        CMD.selectOption sess h
    unselectOption sess (WebKitNode h) = do
        CMD.unselectOption sess h
    click sess (WebKitNode h) = do
        CMD.click sess h
    rightClick sess (WebKitNode h) = do
        CMD.rightClick sess h
    doubleClick sess (WebKitNode h) = do
        CMD.doubleClick sess h
    hover sess (WebKitNode h) = do
        CMD.hover sess h
    dragTo sess (WebKitNode h1) (WebKitNode h2) = do
        CMD.dragTo sess h1 h2
    tagName sess (WebKitNode h) = do
        CMD.tagName sess h
    isVisible sess (WebKitNode h) = do
        CMD.isVisible sess h
    isChecked sess (WebKitNode h) = do
        CMD.isChecked sess h
    isSelected sess (WebKitNode h) = do
        CMD.isSelected sess h
    isDisabled sess (WebKitNode h) = do
        CMD.isDisabled sess h
    path sess (WebKitNode h) = do
        CMD.path sess h
    trigger sess (WebKitNode h) event = do
        CMD.trigger sess h event
    nodeEq sess (WebKitNode h1) (WebKitNode h2) = do
        CMD.nodeEq sess h1 h2
