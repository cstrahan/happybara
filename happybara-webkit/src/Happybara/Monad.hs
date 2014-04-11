{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Happybara.Monad where

import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (isDigit)
import           Data.List                   (isPrefixOf, sort)
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Data.Typeable
import qualified Data.Word8                  as BS

import           Control.Applicative
import           Control.Concurrent
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

import           Happybara.Driver            (Driver, FrameSelector (..), Node,
                                              NodeValue (..))
import qualified Happybara.Driver            as D
import           Happybara.Exceptions
import           Happybara.WebKit.Exceptions
import qualified Happybara.XPath             as X
import           Paths_happybara_webkit      (getDataFileName)

data Exactness = Exact
               | PreferExact
               | Inexact
               deriving (Eq, Ord, Show)

data SingleMatchStrategy = MatchOne
                         | MatchFirst

data HappybaraState sess = HappybaraState { hsDriver              :: sess
                                          , hsWait                :: Double
                                          , hsExactness           :: Exactness
                                          , hsIsSynced            :: Bool
                                          , hsSingleMatchStrategy :: SingleMatchStrategy
                                          , hsCurrentNode         :: Maybe (Node sess)
                                          }

type Happybara sess a = HappybaraT sess IO a
newtype HappybaraT sess m a = HappybaraT { unHappybaraT :: StateT (HappybaraState sess) m a }
                              deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadBase b m) => MonadBase b (HappybaraT sess m) where
    liftBase = lift . liftBase

instance MonadTransControl (HappybaraT sess) where
    newtype StT (HappybaraT sess) a = StHappybara {unStHappybara :: StT (StateT (HappybaraState sess)) a}
    liftWith = defaultLiftWith HappybaraT unHappybaraT StHappybara
    restoreT = defaultRestoreT HappybaraT unStHappybara
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (HappybaraT sess m) where
    newtype StM ((HappybaraT sess) m) a = StMHappybara {unStMHappybara :: ComposeSt (HappybaraT sess) m a}
    liftBaseWith = defaultLiftBaseWith StMHappybara
    restoreM     = defaultRestoreM unStMHappybara
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

runHappybara :: (Driver sess)
             => sess -> Happybara sess a -> IO a
runHappybara sess act =
    evalStateT (unHappybaraT act) initialState
  where
    initialState = HappybaraState { hsDriver = sess
                                  , hsWait = 2
                                  , hsExactness = Inexact
                                  , hsIsSynced = False
                                  , hsSingleMatchStrategy = MatchOne
                                  , hsCurrentNode = Nothing
                                  }

setWait :: (Monad m) => Double -> HappybaraT sess m ()
setWait time =
    HappybaraT $ modify $ \s -> s { hsWait = time }

getWait :: (Functor m, Monad m) => HappybaraT sess m Double
getWait =
    HappybaraT $ hsWait <$> get

setExactness :: (Monad m) => Exactness -> HappybaraT sess m ()
setExactness exact =
    HappybaraT $ modify $ \s -> s { hsExactness = exact }

getExactness :: (Functor m, Monad m) => HappybaraT sess m Exactness
getExactness =
    HappybaraT $ hsExactness <$> get

setDriver :: (Monad m) => sess -> HappybaraT sess m ()
setDriver d =
    HappybaraT $ modify $ \s -> s { hsDriver = d }

getDriver :: (Functor m, Monad m) => HappybaraT sess m sess
getDriver =
    HappybaraT $ hsDriver <$> get

setSingleMatchStrategy :: (Monad m) => SingleMatchStrategy -> HappybaraT sess m ()
setSingleMatchStrategy strategy =
    HappybaraT $ modify $ \s -> s { hsSingleMatchStrategy = strategy }

getSingleMatchStrategy :: (Functor m, Monad m) => HappybaraT sess m SingleMatchStrategy
getSingleMatchStrategy =
    HappybaraT $ hsSingleMatchStrategy <$> get

getCurrentNode :: (Driver sess, Functor m, Monad m) => HappybaraT sess m (Maybe (Node sess))
getCurrentNode =
    HappybaraT $ hsCurrentNode <$> get

withinNode :: (Driver sess, Functor m, Monad m)
           => Node sess -> HappybaraT sess m a -> HappybaraT sess m a
withinNode newNode act = do
    oldNode <- getCurrentNode
    HappybaraT $ modify $ \s -> s { hsCurrentNode = Just newNode }
    res <- act
    HappybaraT $ modify $ \s -> s { hsCurrentNode = oldNode }
    return res

withinPage :: (Driver sess, Functor m, Monad m)
           => HappybaraT sess m a -> HappybaraT sess m a
withinPage act = do
    oldNode <- getCurrentNode
    HappybaraT $ modify $ \s -> s { hsCurrentNode = Nothing }
    res <- act
    HappybaraT $ modify $ \s -> s { hsCurrentNode = oldNode }
    return res

synchronize :: (Functor m, Monad m, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
            => HappybaraT sess m a -> HappybaraT sess m a
synchronize act = do
    synced <- HappybaraT $ hsIsSynced <$> get
    if synced
      then act
      else do
          startTime <- liftIO getCurrentTime
          maxWait <- getWait
          HappybaraT $ modify $ \s -> s { hsIsSynced = True }
          res <- retry startTime maxWait
          HappybaraT $ modify $ \s -> s { hsIsSynced = False }
          return res
  where
    delayMicros = 50000 -- 0.05 seconds
    retry startTime maxWait =
        act `catch` (\(e :: InvalidElementException) -> do
            currenTime <- liftBase getCurrentTime
            let elapsedSeconds = fromRational $ toRational $ diffUTCTime startTime currenTime
            if elapsedSeconds > maxWait
              then throw e
              else do
                  liftBase $ threadDelay delayMicros
                  retry startTime maxWait)

-- wrapped driver methods

currentUrl :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m Text
currentUrl = do
    driver <- getDriver
    liftBase $ D.currentUrl driver

visit :: (Driver sess, Monad m, MonadBase IO m) => Text -> HappybaraT sess m ()
visit url = do
    driver <- getDriver
    liftBase $ D.visit driver url

findXPath :: (Driver sess, Monad m, MonadBase IO m) => Text -> HappybaraT sess m [Node sess]
findXPath xpath = do
    driver <- getDriver
    currentNode <- getCurrentNode
    case currentNode of
        Just node -> liftBase $ D.findXPathRel driver node xpath
        Nothing   -> liftBase $ D.findXPath driver xpath

findCSS :: (Driver sess, Monad m, MonadBase IO m) => Text -> HappybaraT sess m [Node sess]
findCSS css = do
    driver <- getDriver
    currentNode <- getCurrentNode
    case currentNode of
        Just node -> liftBase $ D.findCSSRel driver node css
        Nothing   -> liftBase $ D.findCSS driver css

html :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m Text
html = do
    driver <- getDriver
    liftBase $ D.html driver

goBack :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m ()
goBack = do
    driver <- getDriver
    liftBase $ D.goBack driver

goForward :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m ()
goForward = do
    driver <- getDriver
    liftBase $ D.goForward driver

executeScript :: (Driver sess, Monad m, MonadBase IO m) => Text -> HappybaraT sess m ()
executeScript script = do
    driver <- getDriver
    liftBase $ D.executeScript driver script

evaluateScript :: (Driver sess, Monad m, MonadBase IO m) => Text -> HappybaraT sess m Value
evaluateScript script = do
    driver <- getDriver
    liftBase $ D.evaluateScript driver script

saveScreenshot :: (Driver sess, Monad m, MonadBase IO m) => Text -> Int -> Int -> HappybaraT sess m ()
saveScreenshot path width height = do
    driver <- getDriver
    liftBase $ D.saveScreenshot driver path width height

responseHeaders :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m ResponseHeaders
responseHeaders = do
    driver <- getDriver
    liftBase $ D.responseHeaders driver

statusCode :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m Status
statusCode = do
    driver <- getDriver
    liftBase $ D.statusCode driver

withinFrame :: (Driver sess, Monad m, MonadBase IO m, MonadBaseControl IO m)
            => FrameSelector -> HappybaraT sess m Status -> HappybaraT sess m Status
withinFrame frameId act = do
    driver <- getDriver
    bracket_
        (liftBase $ D.setFrameFocus driver frameId)
        (liftBase $ D.setFrameFocus driver DefaultFrame)
        (act)

withinWindow :: (Driver sess, Monad m, MonadBase IO m, MonadBaseControl IO m)
            => FrameSelector -> HappybaraT sess m Status -> HappybaraT sess m Status
withinWindow windowId act = error "NOT IMPLEMENTED"

reset :: (Driver sess, Monad m, MonadBase IO m) => HappybaraT sess m ()
reset = do
    driver <- getDriver
    liftBase $ D.reset driver

allText :: (Driver sess, Monad m, MonadBase IO m)
        => Node sess -> HappybaraT sess m Text
allText node = do
    driver <- getDriver
    liftBase $ D.allText driver node

visibleText :: (Driver sess, Monad m, MonadBase IO m)
            => Node sess -> HappybaraT sess m Text
visibleText node = do
    driver <- getDriver
    liftBase $ D.visibleText driver node

attr :: (Driver sess, Monad m, MonadBase IO m)
     => Node sess -> Text -> HappybaraT sess m (Maybe Text)
attr node name = do
    driver <- getDriver
    liftBase $ D.attr driver node name

getValue :: (Driver sess, Monad m, MonadBase IO m)
         => Node sess -> HappybaraT sess m NodeValue
getValue node = do
    driver <- getDriver
    liftBase $ D.getValue driver node

setValue :: (Driver sess, Monad m, MonadBase IO m)
         => Node sess -> NodeValue -> HappybaraT sess m ()
setValue node val = do
    driver <- getDriver
    liftBase $ D.setValue driver node val

selectOption :: (Driver sess, Monad m, MonadBase IO m)
             => Node sess -> HappybaraT sess m ()
selectOption node = do
    driver <- getDriver
    liftBase $ D.selectOption driver node

unselectOption :: (Driver sess, Monad m, MonadBase IO m)
               => Node sess -> HappybaraT sess m ()
unselectOption node = do
    driver <- getDriver
    liftBase $ D.unselectOption driver node

click :: (Driver sess, Monad m, MonadBase IO m)
      => Node sess -> HappybaraT sess m ()
click node = do
    driver <- getDriver
    liftBase $ D.click driver node

rightClick :: (Driver sess, Monad m, MonadBase IO m)
           => Node sess -> HappybaraT sess m ()
rightClick node = do
    driver <- getDriver
    liftBase $ D.rightClick driver node

doubleClick :: (Driver sess, Monad m, MonadBase IO m)
            => Node sess -> HappybaraT sess m ()
doubleClick node = do
    driver <- getDriver
    liftBase $ D.doubleClick driver node

hover :: (Driver sess, Monad m, MonadBase IO m)
      => Node sess -> HappybaraT sess m ()
hover node = do
    driver <- getDriver
    liftBase $ D.hover driver node

dragTo :: (Driver sess, Monad m, MonadBase IO m)
       => Node sess -> Node sess -> HappybaraT sess m ()
dragTo node other = do
    driver <- getDriver
    liftBase $ D.dragTo driver node other

tagName :: (Driver sess, Monad m, MonadBase IO m)
        => Node sess -> HappybaraT sess m Text
tagName node = do
    driver <- getDriver
    liftBase $ D.tagName driver node

isVisible :: (Driver sess, Monad m, MonadBase IO m)
          => Node sess -> HappybaraT sess m Bool
isVisible node = do
    driver <- getDriver
    liftBase $ D.isVisible driver node

isChecked :: (Driver sess, Monad m, MonadBase IO m)
          => Node sess -> HappybaraT sess m Bool
isChecked node = do
    driver <- getDriver
    liftBase $ D.isChecked driver node

isSelected :: (Driver sess, Monad m, MonadBase IO m)
           => Node sess -> HappybaraT sess m Bool
isSelected node = do
    driver <- getDriver
    liftBase $ D.isSelected driver node

isDisabled :: (Driver sess, Monad m, MonadBase IO m)
           => Node sess -> HappybaraT sess m Bool
isDisabled node = do
    driver <- getDriver
    liftBase $ D.isDisabled driver node

path :: (Driver sess, Monad m, MonadBase IO m)
     => Node sess -> HappybaraT sess m Text
path node = do
    driver <- getDriver
    liftBase $ D.path driver node

trigger :: (Driver sess, Monad m, MonadBase IO m)
        => Node sess -> Text -> HappybaraT sess m ()
trigger node event = do
    driver <- getDriver
    liftBase $ D.trigger driver node event

nodeEq :: (Driver sess, Monad m, MonadBase IO m)
        => Node sess -> Node sess -> HappybaraT sess m Bool
nodeEq node other = do
    driver <- getDriver
    liftBase $ D.nodeEq driver node other
