{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, BangPatterns, OverloadedStrings #-}

import Happybara.WebKit.Classes
import Happybara.WebKit.Monad

import Control.Monad.State

import Data.Text as T

main :: IO ()
main = do
    serverPath <- defaultServerPath
    withSession serverPath $ \sess -> do
        visit sess "http://google.com"
        h <- currentUrl sess
        liftIO $ putStr $ T.unpack h
        return ()
