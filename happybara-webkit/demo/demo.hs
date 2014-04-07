{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import           Happybara.Classes
import           Happybara.Query
import           Happybara.WebKit.Driver
import           Happybara.WebKit.Session

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
        btns <- findAll sess (inexact $ button "I'm Feeling Lucky" [])
        forM_ btns $ \btn -> do
            text <- allText sess btn
            puts text
        return ()
