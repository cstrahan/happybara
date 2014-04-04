{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, BangPatterns, OverloadedStrings #-}

module Happybara.WebKit.Classes where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Word8 as BS
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (maybe)
import Data.List (isPrefixOf)
import Data.ByteString (ByteString)
import Data.Aeson

import Control.Monad.Trans.Control
import Control.Applicative
import Control.Monad

import System.IO
import System.Timeout
import System.Process

import Network.Socket
import Network.BSD
import Network.HTTP.Types (Header, ResponseHeaders, Status)

import System.Info (os)

import Paths_happybara_webkit (getDataFileName)

data Session = Session { sock       :: Socket
                       , handle     :: Handle
                       , procHandle :: ProcessHandle
                       }

data NodeValue = SingleValue Text
               | MultiValue [Text]

-- TODO: Support Nodes. Maybe use data family?
data FrameId = FrameIndex Int
             | FrameName Text
             | NoFrame

data Query = ByCSS Text
           | ByXPath Text

class MonadBaseControl IO m => Driver m where
    data Node m :: *
    {- currentNode     :: m (Node m) -}
    {- setCurrentNode  :: Node m -> m () -}
    currentUrl      :: m Text
    visit           :: Text -> m ()
    findXPath       :: Text -> m [Node m]
    findCSS         :: Text -> m [Node m]
    html            :: m Text
    goBack          :: m ()
    goForward       :: m ()
    executeScript   :: Text -> m ()
    evaluateScript  :: Text -> m Value
    saveScreenshot  :: Text -> Int -> Int -> m ()
    responseHeaders :: m ResponseHeaders
    statusCode      :: m Status
    withinFrame     :: FrameId -> m a -> m a
    withinWindow    :: Text -> m a -> m a
    reset           :: m ()
    findXPathRel    :: Node m -> Text -> m [Node m]
    findCSSRel      :: Node m -> Text -> m [Node m]
    allText         :: Node m -> m Text
    visibleText     :: Node m -> m Text
    attr            :: Node m -> Text -> m (Maybe Text)
    getValue        :: Node m -> m NodeValue
    setValue        :: Node m -> NodeValue -> m ()
    selectOption    :: Node m -> m ()
    unselectOption  :: Node m -> m ()
    click           :: Node m -> m ()
    rightClick      :: Node m -> m ()
    doubleClick     :: Node m -> m ()
    hover           :: Node m -> m ()
    dragTo          :: Node m -> Node m -> m ()
    tagName         :: Node m -> m Text
    isVisible       :: Node m -> m Bool
    isChecked       :: Node m -> m Bool
    isSelected      :: Node m -> m Bool
    isDisabled      :: Node m -> m Bool
    path            :: Node m -> m Text
    trigger         :: Node m -> Text -> m ()
    nodeEq          :: Node m -> Node m -> m Bool

webkitServerStartTimeout = 15 * 1000000

defaultServerPath :: IO FilePath
defaultServerPath = getDataFileName $ if os == "mingw32"
                                        then "webkit_server.exe"
                                        else "webkit_server"

mkSession :: FilePath -> IO Session
mkSession serverPath = do
    (_, sout, serr, p) <- runInteractiveProcess serverPath [] Nothing Nothing
    mport <- timeout webkitServerStartTimeout (parsePort <$> hGetLine sout)
    let port = maybe noDetectError id mport
    addr <- head <$> getAddrInfo Nothing (Just "localhost") (Just $ show port)
    (s, h) <- conn addr
    return $ Session { sock = s, handle = h, procHandle = p }
  where
    conn addr = do
        s <- socket (addrFamily addr) Stream defaultProtocol
        setSocketOption s KeepAlive 1
        connect s (addrAddress addr)
        h <- socketToHandle s WriteMode
        hSetBuffering h (BlockBuffering Nothing)
        return (s, h)
    parsePort :: String -> Int
    parsePort str =
        if prefix `isPrefixOf` str
          then port
          else noDetectError
      where
        prefix = "listening on port: "
        digits = takeWhile isDigit . drop (length prefix)
        port = read $ digits str

noDetectError = error "could not detect webkit_server port"
