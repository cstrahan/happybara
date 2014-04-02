{-# LANGUAGE OverloadedStrings#-}

module Happybara.WebKit.Classes
       (
       ) where


{- import qualified Data.ByteString as BS -}
import qualified Data.ByteString.Char8 as BS
import qualified Data.Word8 as BS
import Data.Char (isDigit)
import Data.Maybe (maybe)
import Data.List (isPrefixOf)
import Data.ByteString (ByteString)

import Control.Applicative
import Control.Monad

import System.IO
import System.Timeout
import System.Process

import Network.Socket
import Network.BSD

import System.Info (os)

import Paths_happybara_webkit (getDataFileName)

data Session = Session { sock       :: Socket
                       , handle     :: Handle
                       , procHandle :: ProcessHandle
                       }


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

command :: Session -> ByteString -> [ByteString] -> IO ByteString
command sess cmd args = do
    BS.hPutStrLn h cmd
    BS.hPutStrLn h (BS.pack . show . length $ args)
    forM_ args $ \arg -> do
        BS.putStrLn (BS.pack . show . BS.length $ arg)
        BS.putStr arg
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

noDetectError = error "could not detect webkit_server port"
