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

getSession :: Browser Session
getSession = Browser $ get

runBrowser :: Browser a -> IO a
runBrowser (Browser m) = do
    serverPath <- defaultServerPath
    sess <- mkSession serverPath
    evalStateT m sess

instance Driver Browser where
    data Node Browser = BrowserNode !Text
    currentUrl = do
        sess <- getSession
        liftIO $ CMD.currentUrl sess
    visit url = do
        sess <- getSession
        liftIO $ CMD.visit sess url
    findXPath query = do
        sess <- getSession
        handles <- liftIO $ CMD.findXPath sess query
        return $ map BrowserNode handles
    findCSS query = do
        sess <- getSession
        handles <- liftIO $ CMD.findCSS sess query
        return $ map BrowserNode handles
    html = do
        sess <- getSession
        liftIO $ CMD.body sess
    goBack = error "NOT IMPLEMENTED"
    goForward = error "NOT IMPLEMENTED"
    executeScript script = do
        sess <- getSession
        liftIO $ CMD.executeScript sess script
    evaluateScript script = do
        sess <- getSession
        liftIO $ CMD.evaluateScript sess script
    saveScreenshot path width height = do
        sess <- getSession
        liftIO $ CMD.render sess path width height
    responseHeaders = do
        sess <- getSession
        liftIO $ CMD.responseHeaders sess
    statusCode = do
        sess <- getSession
        liftIO $ toEnum <$> CMD.statusCode sess
    withinFrame frameId m = do
        sess <- getSession
        liftIO $ CMD.setFrameFocus sess frameId
        res <- m
        liftIO $ CMD.setFrameFocus sess NoFrame
        return res
    withinWindow name m = error "NOT IMPLEMENTED"
    reset = do
        sess <- getSession
        liftIO $ CMD.reset sess
    findXPathRel (BrowserNode h) query = do
        sess <- getSession
        handles <- liftIO $ CMD.findXPathRel sess h query
        return $ map BrowserNode handles
    findCSSRel (BrowserNode h) query = do
        sess <- getSession
        handles <- liftIO $ CMD.findCSSRel sess h query
        return $ map BrowserNode handles
    allText (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.allText sess h
    visibleText (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.visibleText sess h
    attr (BrowserNode h) name = do
        sess <- getSession
        liftIO $ CMD.attr sess h name
    getValue (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.value sess h
    setValue (BrowserNode h) val = do
        sess <- getSession
        liftIO $ CMD.set sess h val
    selectOption (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.selectOption sess h
    unselectOption (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.unselectOption sess h
    click (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.click sess h
    rightClick (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.rightClick sess h
    doubleClick (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.doubleClick sess h
    hover (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.hover sess h
    dragTo (BrowserNode h1) (BrowserNode h2) = do
        sess <- getSession
        liftIO $ CMD.dragTo sess h1 h2
    tagName (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.tagName sess h
    isVisible (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.isVisible sess h
    isChecked (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.isChecked sess h
    isSelected (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.isSelected sess h
    isDisabled (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.isDisabled sess h
    path (BrowserNode h) = do
        sess <- getSession
        liftIO $ CMD.path sess h
    trigger (BrowserNode h) event = do
        sess <- getSession
        liftIO $ CMD.trigger sess h event
    nodeEq (BrowserNode h1) (BrowserNode h2) = do
        sess <- getSession
        liftIO $ CMD.nodeEq sess h1 h2
