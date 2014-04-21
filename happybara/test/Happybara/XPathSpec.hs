{-# LANGUAGE OverloadedStrings #-}

module Happybara.XPathSpec where

import           Test.Hspec

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Control.Monad

import           Text.Parsec.Prim (runParser)

import qualified Happybara.CSS    as C
import qualified Happybara.XPath  as X

spec :: Spec
spec = do
    describe "CSS conversion" $ do
        runExamples

runExamples =
    forM_ examples $ \(css, expected) -> do
        it (T.unpack css) $ do
            print $ X.fromCSS css

examples =
    [ -- simple
      ("a", undefined)
    , ("a.b", undefined)
    , (".a", undefined)
    , (".a#b", undefined)
    , (".a#b.c", undefined)

      -- unions
    , ("a, b", undefined)

      -- attrs
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

      -- string escaping
    , ("a[b=\"c \\\"d\\\" e\"]", undefined)
    , ("a[b='c \\'d\\' e\']", undefined)

      -- pseudo classes
    , ("a:first-child", undefined)
    , ("a:first-of-type", undefined)
    , ("a:last-of-type", undefined)

      -- pseudo funcs
    , ("a:not(b)", undefined)
    , ("a:has(b)", undefined)

    , ("a:nth-child(3)", undefined)
    , ("a:nth-last-child(3)", undefined)

    , ("a:nth-of-type(odd)", undefined)
    , ("a:nth-of-type(even)", undefined)
    , ("a:nth-of-type(1n+2)", undefined)
    , ("a:nth-of-type(3)", undefined)
    , ("a:nth-of-type(2n)", undefined)

    , ("a:nth-last-of-type(odd)", undefined)
    , ("a:nth-last-of-type(even)", undefined)
    , ("a:nth-last-of-type(1n+2)", undefined)
    , ("a:nth-last-of-type(3)", undefined)
    , ("a:nth-last-of-type(2n)", undefined)

    , ("a:nth-last-child(3)", undefined)

    , ("a:nth-child(3)", undefined)

      -- combinators
    , ("a b", undefined)
    , ("a > b", undefined)
    , ("a + b", undefined)
    , ("a ~ b", undefined)

      -- multiple combinators
    , ("a b c", undefined)
    ]
