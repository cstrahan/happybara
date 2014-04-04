{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, BangPatterns, OverloadedStrings,
             ExistentialQuantification, DeriveDataTypeable #-}

module Happybara.WebKit.Monad where

import Data.Typeable
import Data.Aeson
import Data.Text (Text)
import Data.ByteString (ByteString)

import Network.HTTP.Types

import Control.Monad.State
import Control.Exception
import Control.Applicative
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
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadBase IO Browser where
    liftBase = Browser . liftBase

instance MonadBaseControl IO Browser where
    data StM Browser a = StBrowser {unStBrowser :: StM (StateT Session IO) a}

    liftBaseWith f = Browser $
      liftBaseWith $ \runInBase ->
      f (\(Browser sT) -> liftM StBrowser . runInBase $ sT)

    restoreM = Browser . restoreM . unStBrowser

runBrowser :: Browser a -> IO a
runBrowser (Browser m) = do
    serverPath <- defaultServerPath
    sess <- mkSession serverPath
    evalStateT m sess

instance Driver Session where
    data Node Session = WebKitNode !Text
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
    withinFrame sess frameId act = do
        bracket_
            (CMD.setFrameFocus sess frameId)
            (CMD.setFrameFocus sess NoFrame)
            (act)
    withinWindow sess name m = error "NOT IMPLEMENTED"
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
