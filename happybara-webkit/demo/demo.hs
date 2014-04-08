{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import           Happybara.Classes
import           Happybara.Query
import           Happybara.WebKit.Driver
import           Happybara.WebKit.Session
import qualified Happybara.XPath          as XPath

import           Control.Applicative
import           Control.Monad.State

import qualified Data.ByteString.Char8    as BS
import           Data.Text                as T
import           Data.Text.Encoding       as T

import qualified System.IO                as IO

puts txt = do
    BS.hPutStrLn IO.stdout $ T.encodeUtf8 txt

main :: IO ()
main = do
    serverPath <- defaultServerPath
    withSession serverPath $ \sess -> do
        visit sess "http://google.com"

        btn <- findFirstOrFail sess (inexact $ button "I'm Feeling Lucky" [disabled False])
        SingleValue value <- getValue sess btn
        puts $ T.concat [ "Button found: ", value ]

        click sess btn

        url <- currentUrl sess
        puts $ T.concat [ "New url: ", url ]

        return ()
