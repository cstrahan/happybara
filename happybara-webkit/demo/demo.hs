{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, BangPatterns, OverloadedStrings #-}

import Happybara.WebKit.Classes
import Happybara.WebKit.Monad

import Control.Monad.State

import Data.Text as T

main :: IO ()
main = do
    runBrowser $ do
        visit "http://google.com"
        h <- currentUrl
        liftIO $ putStr $ T.unpack h
        return ()
