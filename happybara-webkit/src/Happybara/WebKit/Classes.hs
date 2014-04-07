{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Happybara.WebKit.Classes where

import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (isDigit)
import           Data.List                   (isPrefixOf)
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

import qualified Happybara.WebKit.XPath      as X
import           Paths_happybara_webkit      (getDataFileName, getLibexecDir)

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

data Session = Session { sock       :: Net.Socket
                       , sockHandle :: Handle
                       , procHandle :: ProcessHandle
                       }

data NodeValue = SingleValue Text
               | MultiValue [Text]

-- TODO: Support Nodes. Maybe use data family?
data FrameSelector = FrameIndex Int
                   | FrameName Text
                   | DefaultFrame
                   -- TODO? | FrameElement

data Exactness = Exact
               | PreferExact
               | Inexact

exact :: (Query q) => q -> q
exact = flip withExactness Exact

preferExact :: (Query q) => q -> q
preferExact = flip withExactness PreferExact

inexact :: (Query q) => q -> q
inexact = flip withExactness Inexact

class Query q where
    type QueryDriver q :: *

    queryDescription :: q -> String

    relativeTo'      :: q -> Maybe (Node (QueryDriver q)) -> q

    withExactness :: q -> Exactness -> q

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

    findFirstOrFail :: (Query q) => (QueryDriver q) -> q -> IO (Node (QueryDriver q))
    findFirstOrFail sess q = do
        mnode <- findFirst sess q
        case mnode of
            Just mnode -> return mnode
            _          -> throw ElementNotFoundException

    findOneOrFail :: (Query q) => (QueryDriver q) -> q -> IO (Node (QueryDriver q))
    findOneOrFail sess q =
        findAll sess q >>= go
      where
        go (node:[]) = return node
        go (node:_)  = throw AmbiguousElementException
        go []        = throw ElementNotFoundException

data SimpleQuery sess where
    MkSimpleQuery :: (Driver sess)
                  => Exactness
                  -> (Bool -> Text)
                  -> (Maybe (Node sess))
                  -> [(Node sess) -> IO Bool]
                  -> Text
                  -> SimpleQuery sess

instance Query (SimpleQuery sess) where
    type QueryDriver (SimpleQuery sess) = sess

    queryDescription (MkSimpleQuery _ _ _ _ desc) =
        T.unpack desc

    relativeTo' (MkSimpleQuery a b _ c d) rel =
        MkSimpleQuery a b rel c d

    withExactness (MkSimpleQuery _ a b c d) exact =
        MkSimpleQuery exact a b c d

    findAll sess (MkSimpleQuery exact xpath mrel preds _) =
        case exact of
            Exact -> do
                find $ xpath True
            PreferExact -> do
                res <- find $ xpath True
                if null res
                  then find $ xpath False
                  else return res
            Inexact -> do
                find $ xpath False
      where
        allM _ []     = return True
        allM f (b:bs) = (f b) >>= (\bv -> if bv then allM f bs else return False)
        pred x = allM ($x) preds
        find x = do
            res <- case mrel of
                       Just node -> findXPathRel sess node x
                       _         -> findXPath sess x
            filterM pred res

link :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
link locator preds =
    MkSimpleQuery PreferExact (X.link locator) Nothing preds "link"

button :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
button locator preds =
    MkSimpleQuery PreferExact (X.button locator) Nothing preds "button"

linkOrButton :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
linkOrButton locator preds =
    MkSimpleQuery PreferExact (X.linkOrButton locator) Nothing preds "linkOrButton"

fieldset :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
fieldset locator preds =
    MkSimpleQuery PreferExact (X.fieldset locator) Nothing preds "fieldset"

field :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
field locator preds =
    MkSimpleQuery PreferExact (X.field locator) Nothing preds "field"

fillableField :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
fillableField locator preds =
    MkSimpleQuery PreferExact (X.fillableField locator) Nothing preds "fillableField"

select :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
select locator preds =
    MkSimpleQuery PreferExact (X.select locator) Nothing preds "select"

checkbox :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
checkbox locator preds =
    MkSimpleQuery PreferExact (X.checkbox locator) Nothing preds "checkbox"

radioButton :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
radioButton locator preds =
    MkSimpleQuery PreferExact (X.radioButton locator) Nothing preds "radioButton"

fileField :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
fileField locator preds =
    MkSimpleQuery PreferExact (X.fileField locator) Nothing preds "fileField"

optgroup :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
optgroup locator preds =
    MkSimpleQuery PreferExact (X.optgroup locator) Nothing preds "optgroup"

option :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
option locator preds =
    MkSimpleQuery PreferExact (X.option locator) Nothing preds "option"

table :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
table locator preds =
    MkSimpleQuery PreferExact (X.table locator) Nothing preds "table"

definitionDescription :: (Driver sess) => Text -> [Node sess -> IO Bool] -> SimpleQuery sess
definitionDescription locator preds =
    MkSimpleQuery PreferExact (const $ X.definitionDescription locator) Nothing preds "definitionDescription"

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

webkitServerStartTimeout = 15 * 1000000

defaultServerPath = getDataFileName $ if os == "mingw32"
                                        then "webkit_server.exe"
                                        else "webkit_server"

withSession :: FilePath -> (Session -> IO a) -> IO a
withSession serverPath fun = do
    bracket
        (mkSession serverPath)
        (closeSession)
        (fun)

mkSession :: FilePath -> IO Session
mkSession serverPath = do
    (_, sout, serr, p) <- runInteractiveProcess serverPath [] Nothing Nothing
    mport <- timeout webkitServerStartTimeout (parsePort <$> hGetLine sout)
    let port = maybe noDetectError id mport
    addr <- head <$> Net.getAddrInfo Nothing (Just "localhost") (Just $ show port)
    (s, h) <- conn addr
    return $ Session { sock = s, sockHandle = h, procHandle = p }
  where
    conn addr = do
        s <- Net.socket (Net.addrFamily addr) Net.Stream defaultProtocol
        Net.setSocketOption s Net.KeepAlive 1
        Net.connect s (Net.addrAddress addr)
        h <- Net.socketToHandle s ReadWriteMode
        hSetBuffering h (BlockBuffering Nothing)
        return (s, h)
    parsePort :: String -> Int
    parsePort str =
        if prefix `isPrefixOf` str
          then port
          else noDetectError
      where
        prefix = "Capybara-webkit server started, listening on port: "
        digits = takeWhile isDigit . drop (length prefix)
        port = read $ digits str

closeSession :: Session -> IO ()
closeSession sess = do
    hClose (sockHandle sess)
    terminateProcess (procHandle sess)

noDetectError = error "could not detect webkit_server port"
