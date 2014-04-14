{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
module Happybara.Monad
    ( -- * Happybara Monad
      HappybaraT(..)
    , Happybara
    , runHappybaraT
    , runHappybara
      -- * Monad Settings & State
    , Exactness(..)
    , SingleMatchStrategy(..)
    , setWait
    , getWait
    , setExactness
    , getExactness
    , setSingleMatchStrategy
    , getSingleMatchStrategy
    , getDriver
    , withDriver
    , getCurrentNode
    , HappybaraState(..)
      -- * Browser State
    , visit
    , currentUrl
    , responseHeaders
    , statusCode
    , html
    , goBack
    , goForward
    , reset
    , saveScreenshot
      -- * Scoping
    , withinNode
    , withinPage
    , withinFrame
    , withinWindow
      -- * JavaScript Execution
    , executeScript
    , evaluateScript
      -- * Primitive Queries
    , findXPath
    , findCSS
      -- * Node Manipulation
    , allText
    , visibleText
    , attr
    , getValue
    , setValue
    , selectOption
    , unselectOption
    , click
    , rightClick
    , doubleClick
    , hover
    , dragTo
    , tagName
    , isVisible
    , isChecked
    , isSelected
    , isDisabled
    , path
    , trigger
    , nodeEq
      -- * Async Support
    , synchronize
    ) where

import           Data.Aeson
import           Data.Text                   (Text)
import           Data.Time.Clock

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

import           Network.HTTP.Types          (Header, ResponseHeaders, Status)

import           Happybara.Driver            (Driver, FrameSelector (..), Node,
                                              NodeValue (..))
import qualified Happybara.Driver            as D
import           Happybara.Exceptions

-- | The Happybara monad transformer.
--
-- Requirements:
--
-- * The /sess/ session type must be an instance of 'Driver'.
--
-- * The inner monad /m/ must be an instance of 'MonadBase' 'IO' /m/, 'MonadIO' /m/, and
-- 'MonadBaseControl' 'IO' /m/.
newtype HappybaraT sess m a = HappybaraT { unHappybaraT :: StateT (HappybaraState sess) m a }
                              deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | If you don't want to transform an existing monad, this type synonym
-- conveniently sets the inner monad to 'IO'.
type Happybara sess a = HappybaraT sess IO a

-- | The exactness requirement when using the 'Happybara.Query.Query' DSL.
data Exactness = Exact       -- ^ Find elements that match exactly.
               | PreferExact -- ^ First try to find exact matches;
                             -- if that fails, fall back to inexact matches.
               | Inexact     -- ^ Find all elements that partially match - e.g.
                             -- the given string is infix of (but not necessarily equal to)
                             -- whatever property (id, attribute, etc) is being queried over.
               deriving (Eq, Ord, Show)

-- | This controls the 'Happybara.Query.Query' behavior of 'Happybara.Query.findOrFail' in
-- the presence of multiple matches.
data SingleMatchStrategy = MatchFirst -- ^ If no elements matched, throw 'ElementNotFoundException';
                                      -- otherwise, return the first matching element.
                         | MatchOne   -- ^ If no elements matched, throw 'ElementNotFoundException';
                                      -- if more than element matches, throw 'AmbiguousElementException'.

data HappybaraState sess = HappybaraState { hsDriver              :: sess
                                          , hsWait                :: Double
                                          , hsExactness           :: Exactness
                                          , hsIsSynced            :: Bool
                                          , hsSingleMatchStrategy :: SingleMatchStrategy
                                          , hsCurrentNode         :: Maybe (Node sess)
                                          }

instance (MonadBase b m) => MonadBase b (HappybaraT sess m) where
    liftBase = lift . liftBase

instance MonadTransControl (HappybaraT sess) where
    newtype StT (HappybaraT sess) a = StHappybara {unStHappybara :: StT (StateT (HappybaraState sess)) a}
    liftWith = defaultLiftWith HappybaraT unHappybaraT StHappybara
    restoreT = defaultRestoreT HappybaraT unStHappybara
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (HappybaraT sess m) where
    newtype StM (HappybaraT sess m) a = StMHappybara {unStMHappybara :: ComposeSt (HappybaraT sess) m a}
    liftBaseWith = defaultLiftBaseWith StMHappybara
    restoreM     = defaultRestoreM unStMHappybara
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

-- | Evaluate the happybara computation.
runHappybara :: (Driver sess) => sess -> Happybara sess a -> IO a
runHappybara = runHappybaraT

-- | Evaluate the happybara computation.
runHappybaraT :: (Driver sess, MonadIO m, MonadBase IO m, MonadBaseControl IO m)
              => sess -> HappybaraT sess m a -> m a
runHappybaraT sess act =
    evalStateT (unHappybaraT act) initialState
  where
    initialState = HappybaraState { hsDriver = sess
                                  , hsWait = 2
                                  , hsExactness = Inexact
                                  , hsIsSynced = False
                                  , hsSingleMatchStrategy = MatchOne
                                  , hsCurrentNode = Nothing
                                  }

-- | Set the number of seconds to wait between retrying an action. See
-- 'synchronize'.
setWait :: (Monad m) => Double -> HappybaraT sess m ()
setWait time =
    HappybaraT $ modify $ \s -> s { hsWait = time }

-- | Get the number of seconds to wait between retrying an action. See
-- 'synchronize'.
getWait :: (Functor m, Monad m) => HappybaraT sess m Double
getWait =
    HappybaraT $ hsWait <$> get

-- | Set the required level of exactness for queries. See 'Exactness'.
setExactness :: (Monad m) => Exactness -> HappybaraT sess m ()
setExactness exact =
    HappybaraT $ modify $ \s -> s { hsExactness = exact }

-- | Get the required level of exactness for queries. See 'Exactness'.
getExactness :: (Functor m, Monad m) => HappybaraT sess m Exactness
getExactness =
    HappybaraT $ hsExactness <$> get

-- | Use a different driver for the given action.
--
-- /Note:/ This sets the current scope via 'withinPage' before invoking the
-- action, because the current node was acquired from a different driver
-- instance. Similarly, it's a bad idea to return a 'Node' from this new driver,
-- as you might try to use it with the wrong driver instance.
withDriver :: (Driver sess, Functor m, Monad m)
           => sess -> HappybaraT sess m a -> HappybaraT sess m a
withDriver driver act = do
    oldDriver <-getDriver
    HappybaraT $ modify $ \s -> s { hsDriver = driver }
    result <- withinPage act
    HappybaraT $ modify $ \s -> s { hsDriver = oldDriver }
    return result

getDriver :: (Functor m, Monad m) => HappybaraT sess m sess
getDriver =
    HappybaraT $ hsDriver <$> get

-- | Set the query matching strategy. See 'SingleMatchStrategy'.
setSingleMatchStrategy :: (Monad m) => SingleMatchStrategy -> HappybaraT sess m ()
setSingleMatchStrategy strategy =
    HappybaraT $ modify $ \s -> s { hsSingleMatchStrategy = strategy }

-- | Get the query matching strategy. See 'SingleMatchStrategy'.
getSingleMatchStrategy :: (Functor m, Monad m) => HappybaraT sess m SingleMatchStrategy
getSingleMatchStrategy =
    HappybaraT $ hsSingleMatchStrategy <$> get

-- | Get the node that all queries are currently relative to.
getCurrentNode :: (Driver sess, Functor m, Monad m) => HappybaraT sess m (Maybe (Node sess))
getCurrentNode =
    HappybaraT $ hsCurrentNode <$> get

-- | Make all queries relative to the supplied node within the given action.
withinNode :: (Driver sess, Functor m, Monad m)
           => Node sess -> HappybaraT sess m a -> HappybaraT sess m a
withinNode newNode act = do
    oldNode <- getCurrentNode
    HappybaraT $ modify $ \s -> s { hsCurrentNode = Just newNode }
    res <- act
    HappybaraT $ modify $ \s -> s { hsCurrentNode = oldNode }
    return res

-- | Make all queries relative to the document in the given action.
withinPage :: (Driver sess, Functor m, Monad m)
           => HappybaraT sess m a -> HappybaraT sess m a
withinPage act = do
    oldNode <- getCurrentNode
    HappybaraT $ modify $ \s -> s { hsCurrentNode = Nothing }
    res <- act
    HappybaraT $ modify $ \s -> s { hsCurrentNode = oldNode }
    return res

-- | Invoke the given action until:
--
-- * The action no longer throws 'InvalidElementException', or
--
-- * The total duration of the attempts excedes the number of seconds specified by 'getWait', in
-- which case the exception is rethrown.
--
-- A couple notes:
--
-- * The action is retried every 0.05 seconds.
--
-- * To prevent exponential retrying, any inner calls to 'synchronize' are
-- ignored.
--
-- * Unless you're doing something advanced,
-- like implementing custom 'Happybara.Query.Query' instances,
-- you probably don't need to invoke this directly.
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
            let elapsedSeconds = fromRational $ toRational $ currenTime `diffUTCTime` startTime
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
