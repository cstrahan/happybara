{-# LANGUAGE OverloadedStrings #-}

module Happybara.WebKit.Commands where

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS (fromStrict, toStrict)
import qualified Data.CaseInsensitive       as CI
import           Data.Char                  (isDigit)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (maybe, fromJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Vector                as V

import           Control.Applicative
import           Control.Exception
import           Control.Monad

import           System.IO
import           System.Process
import           System.Timeout

import           Network.BSD
import           Network.HTTP.Types
import           Network.Socket

import           System.Info                (os)

import           Happybara.Classes (FrameSelector(..), NodeValue(..))
import           Happybara.Exceptions
import           Happybara.WebKit.Exceptions
import           Happybara.WebKit.Session (Session(..))

type NodeHandle = Text

data JsonError = JsonError { jsonErrorClass :: String
                           , jsonErrorMessage :: String
                           }

instance FromJSON JsonError where
    parseJSON (Object v) = JsonError <$>
                           v .: "class" <*>
                           v .: "message"
    parseJSON _          = mzero

enc :: Text -> ByteString
enc s = encodeUtf8 s

dec :: IO ByteString -> IO Text
dec s = s >>= (return . decodeUtf8)

toValue :: ByteString -> Value
toValue str = case eitherDecode (BS.fromStrict str) of
                  Left msg  -> error msg
                  Right val -> val

jsonErrorToException :: JsonError -> SomeException
jsonErrorToException (JsonError "NodeNotAttachedError" msg) =
    toException $ NodeNotAttachedException msg
jsonErrorToException (JsonError "InvalidResponseError" msg) =
    toException $ InvalidResponseException msg
jsonErrorToException (JsonError "NoResponseError" msg) =
    toException $ NoResponseException msg
jsonErrorToException (JsonError "ClickFailed" msg) =
    toException $ ClickFailedException msg
jsonErrorToException (JsonError "TimeoutError" msg) =
    toException $ TimeoutException msg
jsonErrorToException (JsonError klass msg) =
    error $ concat [ "Unkown exception type: ", klass ]

command :: Session -> ByteString -> [ByteString] -> IO ByteString
command sess cmd args = do
    BS.hPutStrLn h cmd
    BS.hPutStrLn h (BS.pack . show . length $ args)
    forM_ args $ \arg -> do
        BS.hPutStrLn h (BS.pack . show . BS.length $ arg)
        BS.hPutStr h arg
    hFlush h
    check
    readResponse
  where
    h = sockHandle sess
    check = do
        result <- BS.hGetLine h
        when (result /= "ok") $ do
            response <- readResponse
            (throw . jsonErrorToException . fromJust . decode) $ BS.fromStrict response
    readResponse = do
        len <- (read . BS.unpack) <$> BS.hGetLine h
        if len > 0
          then BS.hGet h len
          else return ""

invoke :: Session -> NodeHandle -> ByteString -> [ByteString] -> IO ByteString
invoke sess h name args =
    command sess "Node" (name:allow_unattached_nodes:enc h:args)
  where
    allow_unattached_nodes = "true"

authenticate :: Session -> Text -> Text -> IO ()
authenticate sess username password =
    void $ command sess "Authenticate" [enc username, enc password]

enableLogging :: Session -> IO ()
enableLogging sess =
    void $ command sess "EnableLogging" []

visit :: Session -> Text -> IO ()
visit sess url =
    void $ command sess "Visit" [enc url]

header :: Session -> Text -> Text -> IO ()
header sess key value =
    void $ command sess "Header" [enc key, enc value]

getTitle :: Session -> IO Text
getTitle sess =
    dec $ command sess "Title" []

findXPath :: Session -> Text -> IO [NodeHandle]
findXPath sess query =
    T.splitOn "," <$> (dec $ command sess "FindXpath" [enc query])

findCSS :: Session -> Text -> IO [NodeHandle]
findCSS sess query =
    T.splitOn "," <$> (dec $ command sess "FindCss" [enc query])

reset :: Session -> IO ()
reset sess =
    void $ command sess "Reset" []

body :: Session -> IO Text
body sess =
    dec $ command sess "Body" []

statusCode :: Session -> IO Int
statusCode sess =
    (return . read . BS.unpack) =<< command sess "Status" []

consoleMessages :: Session -> IO Value
consoleMessages sess =
    (return . toValue) =<< command sess "ConsoleMessages" []

alertMessages :: Session -> IO Value
alertMessages sess =
    (return . toValue) =<< command sess "JavascriptAlertMessages" []

confirmMessages :: Session -> IO Value
confirmMessages sess =
    (return . toValue) =<< command sess "JavascriptConfirmMessages" []

promptMessages :: Session -> IO Value
promptMessages sess =
    (return . toValue) =<< command sess "JavascriptPromptMessages" []

responseHeaders :: Session -> IO ResponseHeaders
responseHeaders sess = do
    str <- command sess "Headers" []
    return $ map parseHeader (BS.lines str)
  where
    parseHeader :: ByteString -> Header
    parseHeader s = let Just i = BS.elemIndex ':' s
                        k = BS.take i s
                        v = BS.drop (i+1) s
                    in (CI.mk k, v)

currentUrl :: Session -> IO Text
currentUrl sess =
    dec $ command sess "CurrentUrl" []

setFrameFocus :: Session -> FrameSelector -> IO ()
setFrameFocus sess frame =
    case frame of
        FrameIndex idx ->
            void $ command sess "FrameFocus" [enc "", BS.pack . show $ idx]
        FrameName name ->
            void $ command sess "FrameFocus" [enc name]
        DefaultFrame ->
            void $ command sess "FrameFocus" []

ignoreSslErrors :: Session -> IO ()
ignoreSslErrors sess =
    void $ command sess "IgnoreSslErrors" []

setSkipImageLoading :: Session -> Bool -> IO ()
setSkipImageLoading sess flag =
    void $ command sess "SetSkipImageLoading" [arg]
  where
    arg = if flag then "true" else "false"

acceptJsConfirms :: Session -> IO ()
acceptJsConfirms sess =
    void $ command sess "SetConfirmAction" ["Yes"]

rejectJsConfirms :: Session -> IO ()
rejectJsConfirms sess =
    void $ command sess "SetConfirmAction" ["No"]

acceptJsPrompts :: Session -> IO ()
acceptJsPrompts sess =
    void $ command sess "SetPromptAction" ["Yes"]

rejectJsPrompts :: Session -> IO ()
rejectJsPrompts sess =
    void $ command sess "SetPromptAction" ["No"]

setPromptTextTo :: Session -> Text -> IO ()
setPromptTextTo sess str =
    void $ command sess "SetPromptText" [enc str]

clearPromptText :: Session -> IO ()
clearPromptText sess =
    void $ command sess "ClearPromptText" []

setUrlBlacklist :: Session -> [Text] -> IO ()
setUrlBlacklist sess urls =
    void $ command sess "SetUrlBlacklist" (map enc urls)

evaluateScript :: Session -> Text -> IO Value
evaluateScript sess script = do
    res <- command sess "Evaluate" [enc script]
    let Array array = toValue $ BS.concat ["[", res, "]"]
    return $ V.head array

executeScript :: Session -> Text -> IO ()
executeScript sess script =
    void $ command sess "Execute" [enc script]

render :: Session -> Text -> Int -> Int -> IO ()
render sess path width height =
    void $ command sess "Render" [enc path, show' width, show' height]
  where
    show' = (BS.pack . show)

setTimeout :: Session -> Int -> IO ()
setTimeout sess seconds =
    void $ command sess "SetTimeout" [BS.pack . show $ seconds]

getTimeout :: Session -> IO Int
getTimeout sess =
    (return . read . BS.unpack) =<< command sess "GetTimeout" []

clearCookies :: Session -> IO ()
clearCookies sess =
    void $ command sess "ClearCookies" []

clearProxy :: Session -> IO ()
clearProxy sess =
    void $ command sess "SetProxy" []

resizeWindow :: Session -> Int -> Int -> IO ()
resizeWindow sess width height =
    void $ command sess "ResizeWindow" [show' width, show' height]
  where
    show' = (BS.pack . show)

getVersion :: Session -> IO Text
getVersion sess =
    dec $ command sess "Version" []


-- NODE FUNCTIONS

allText :: Session -> NodeHandle -> IO Text
allText sess h = dec $ invoke sess h "allText" []

visibleText :: Session -> NodeHandle -> IO Text
visibleText sess h = dec $ invoke sess h "text" []

findXPathRel :: Session -> NodeHandle -> Text -> IO [NodeHandle]
findXPathRel sess h query =
    T.splitOn "," <$> (dec $ invoke sess h "findXpathWithin" [enc query])

findCSSRel :: Session -> NodeHandle -> Text -> IO [NodeHandle]
findCSSRel sess h query =
    T.splitOn "," <$> (dec $ invoke sess h "findCssWithin" [enc query])

attr :: Session -> NodeHandle -> Text -> IO (Maybe Text)
attr sess h name = do
    has <- hasAttr sess h name
    if has
      then do
          val <- (dec $ invoke sess h "attribute" [enc name])
          return $ Just val
      else do
          return Nothing

hasAttr :: Session -> NodeHandle -> Text -> IO Bool
hasAttr sess h name = do
    val <- invoke sess h "hasAttribute" [enc name]
    return $ val == "true"

value :: Session -> NodeHandle -> IO NodeValue
value sess h = do
    isMult <- isMultipleSelect sess h
    if isMult
      then do
          handles <- findXPathRel sess h ".//option"
          selected <- filterM (isSelected sess) handles
          values <- mapM (invoke sess h "value" . (:[]) . enc) selected
          return $ MultiValue selected
      else do
        val <- dec $ invoke sess h "value" []
        return $ SingleValue val

set :: Session -> NodeHandle -> NodeValue -> IO ()
set sess h val =
    case val of
        SingleValue v -> 
            void $ invoke sess h "set" [enc v]
        MultiValue vs -> 
            void $ invoke sess h "set" (map enc vs)

isMultipleSelect :: Session -> NodeHandle -> IO Bool
isMultipleSelect sess h = do
    name <- tagName sess h
    mult <- attr sess h "multiple"
    return $ mult == Just "true"

tagName :: Session -> NodeHandle -> IO Text
tagName sess h = dec $ invoke sess h "tagName" []

isSelected :: Session -> NodeHandle -> IO Bool
isSelected sess h = do
    val <- invoke sess h "selected" []
    return $ val == "true"

isVisible :: Session -> NodeHandle -> IO Bool
isVisible sess h = do
    val <- invoke sess h "visible" []
    return $ val == "true"

isChecked :: Session -> NodeHandle -> IO Bool
isChecked sess h = do
    mult <- attr sess h "checked"
    return $ mult == Just "true"

isDisabled :: Session -> NodeHandle -> IO Bool
isDisabled sess h = do
    name <- tagName sess h
    if (name == "option" || name == "optgroup")
      then do
          dis <- attr sess h "disabled"
          if (dis == Just "true")
            then return True
            else do
                parent <- findXPathRel sess h "parent::*"
                isDisabled sess (head parent)
      else do
          dis <- attr sess h "disabled"
          return $ dis == Just "true"

selectOption :: Session -> NodeHandle -> IO ()
selectOption sess h =
    void $ invoke sess h "selectOption" []

unselectOption :: Session -> NodeHandle -> IO ()
unselectOption sess h = do
    selects <- findXPathRel sess h "ancestor::select"
    if (not . null $ selects)
      then do
          isMult <- isMultipleSelect sess (head selects)
          if isMult
            then void $ invoke sess h "unselectOption" []
            else error "UnselectNotAllowed"
      else error "UnselectNotAllowed"

click :: Session -> NodeHandle -> IO ()
click sess h =
    void $ invoke sess h "leftClick" []

doubleClick :: Session -> NodeHandle -> IO ()
doubleClick sess h =
    void $ invoke sess h "doubleClick" []

rightClick :: Session -> NodeHandle -> IO ()
rightClick sess h =
    void $ invoke sess h "rightClick" []

hover :: Session -> NodeHandle -> IO ()
hover sess h =
    void $ invoke sess h "hover" []

dragTo :: Session -> NodeHandle -> NodeHandle -> IO ()
dragTo sess h1 h2 =
    void $ invoke sess h1 "dragTo" [enc h2]

path :: Session -> NodeHandle -> IO Text
path sess h =
    dec $ invoke sess h "path" []

submit :: Session -> NodeHandle -> IO ()
submit sess h =
    void $ invoke sess h "submit" []

trigger :: Session -> NodeHandle -> Text -> IO ()
trigger sess h event =
    void $ invoke sess h "trigger" [enc event]

isAttached :: Session -> NodeHandle -> IO Bool
isAttached sess h = do
    val <- command sess "Node" ["isAttached", enc h]
    return $ val == "true"

nodeEq :: Session -> NodeHandle -> NodeHandle -> IO Bool
nodeEq sess h1 h2 = do
    val <- invoke sess h1 "equals" [enc h2]
    return $ val == "true"
