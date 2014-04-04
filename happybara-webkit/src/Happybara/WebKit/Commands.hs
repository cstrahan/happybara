{-# LANGUAGE OverloadedStrings #-}

module Happybara.WebKit.Commands where

import           Happybara.WebKit.Classes

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS (fromStrict, toStrict)
import qualified Data.CaseInsensitive       as CI
import           Data.Char                  (isDigit)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (maybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Vector                as V

import           Control.Applicative
import           Control.Monad

import           System.IO
import           System.Process
import           System.Timeout

import           Network.BSD
import           Network.HTTP.Types
import           Network.Socket

import           System.Info                (os)

enc :: Text -> ByteString
enc s = encodeUtf8 s

dec :: IO ByteString -> IO Text
dec s = s >>= (return . decodeUtf8)

command :: Session -> ByteString -> [ByteString] -> IO ByteString
command sess cmd args = do
    BS.hPutStrLn h cmd
    BS.hPutStrLn h (BS.pack . show . length $ args)
    forM_ args $ \arg -> do
        BS.putStrLn (BS.pack . show . BS.length $ arg)
        BS.putStr arg
    hFlush h
    check
    readResponse
  where
    h = handle sess
    check = do
        result <- BS.hGetLine h
        when (result /= "ok") $ do
            response <- readResponse
            fail $ BS.unpack response
    readResponse = do
        len <- (read . BS.unpack) <$> BS.hGetLine h
        if len > 0
          then BS.hGet h len
          else return ""

authenticate :: Session -> Text -> Text -> IO Text
authenticate sess username password =
    dec $ command sess "Authenticate" [enc username, enc password]

enableLogging :: Session -> IO Text
enableLogging sess =
    dec $ command sess "EnableLogging" []

visit :: Session -> Text -> Text -> IO Text
visit sess username password =
    dec $ command sess "Visit" [enc username, enc password]

header :: Session -> Text -> Text -> IO Text
header sess key value =
    dec $ command sess "Header" [enc key, enc value]

getTitle :: Session -> IO Text
getTitle sess =
    dec $ command sess "Title" []

findXPath :: Session -> Text -> IO [Text]
findXPath sess query =
    T.splitOn "," <$> (dec $ command sess "FindXpath" [enc query])

findCSS :: Session -> Text -> IO [Text]
findCSS sess query =
    T.splitOn "," <$> (dec $ command sess "FindCss" [enc query])

reset :: Session -> IO Text
reset sess =
    dec $ command sess "Reset" []

body :: Session -> IO Text
body sess =
    dec $ command sess "Body" []

statusCode :: Session -> IO Int
statusCode sess =
    (return . read . BS.unpack) =<< command sess "Status" []


toValue :: ByteString -> Value
toValue str = case eitherDecode (BS.fromStrict str) of
                  Left msg  -> error msg
                  Right val -> val

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

setFrameFocus :: Session -> FrameId -> IO ()
setFrameFocus sess frame =
    case frame of
        FrameIndex idx ->
            void $ command sess "FrameFocus" [enc "", BS.pack . show $ idx]
        FrameName name ->
            void $ command sess "FrameFocus" [enc name]
        NoFrame ->
            void $ command sess "FrameFocus" []

ignoreSslErrors :: Session -> IO Text
ignoreSslErrors sess =
    dec $ command sess "IgnoreSslErrors" []

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

getVersion :: Session -> IO ()
getVersion sess =
    void $ command sess "Version" []
