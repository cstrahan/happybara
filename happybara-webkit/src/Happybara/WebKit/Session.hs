module Happybara.WebKit.Session where

import           Data.Char                   (isDigit)
import           Data.List                   (isPrefixOf, sort)
import           Data.Maybe                  (maybe)

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception

import           System.FilePath
import           System.IO
import           System.Process
import           System.Timeout

import           Network.BSD                 as Net
import           Network.HTTP.Types          (Header, ResponseHeaders, Status)
import qualified Network.Socket              as Net

import           System.Info                 (os)

import           Paths_happybara_webkit      (getDataFileName, getLibexecDir)

data Session = Session { sock       :: Net.Socket
                       , sockHandle :: Handle
                       , procHandle :: ProcessHandle
                       }

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
