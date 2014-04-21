{-# LANGUAGE OverloadedStrings #-}

module Happybara.CSSSpec where

import           Test.Hspec

import           Control.Monad

import           Text.Parsec.Prim (runParser)

import           Happybara.CSS

parse txt = fromRight $ runParser selectors () "<>" txt
  where
    fromRight (Right x) = x
    fromRight (Left msg) = error $ show msg

spec :: Spec
spec = do
    describe "CSS parsing" $ do
        runExamples

runExamples =
    forM_ examples $ \(css, expected) -> do
        it css $ do
            print $ parse css

examples =
    [ ("a", undefined)
    , ("a.b", undefined)
    , (".a", undefined)
    , (".a#b", undefined)
    , (".a#b.c", undefined)

    , ("a, b", undefined)

    , ("a[b]", undefined)
    , ("a[b=c]", undefined)
    , ("a[b=\"c\"]", undefined)
    , ("a[b='c']", undefined)
    , ("a[b*='c']", undefined)
    , ("a[b!='c']", undefined)
    , ("a[b~='c']", undefined)
    , ("a[b|='c']", undefined)
    , ("a[b^='c']", undefined)
    , ("a[b$='c']", undefined)

    , ("a[b=\"c \\\"d\\\" e\"]", undefined)
    , ("a[b='c \\'d\\' e\']", undefined)

    , ("a:b", undefined)

    , ("a:f()", undefined)
    , ("a:f(3)", undefined)
    , ("a:f(2n)", undefined)
    , ("a:f(-n+2)", undefined)
    , ("a:f(2n+3)", undefined)
    , ("a:f(even)", undefined)
    , ("a:f(odd)", undefined)
    , ("a:f(odd.baz)", undefined)

    , ("a b", undefined)
    , ("a > b", undefined)
    , ("a + b", undefined)
    , ("a ~ b", undefined)

    , ("a b c", undefined)
    ]
