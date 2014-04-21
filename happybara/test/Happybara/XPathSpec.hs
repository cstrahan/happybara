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
        it "prefix works" $ do
            X.fromCSS "./" "a, b" `shouldBe` Right "./a | ./b"

toXPath css =
    fromRight $ X.fromCSS "" css
  where
    fromRight (Right x) = x
    fromRight (Left msg) = error $ T.unpack msg

runExamples = do
    forM_ examples $ \(css, expected) -> do
        it (T.unpack css) $ do
            toXPath css `shouldBe` expected

examples =
    [ -- simple
      ("a",      "a")
    , ("a.b",    "a[contains(concat(' ', normalize-space(./@class), ' '), ' b ')]")
    , (".a",     "*[contains(concat(' ', normalize-space(./@class), ' '), ' a ')]")
    , (".a#b",   "*[contains(concat(' ', normalize-space(./@class), ' '), ' a ') and ./@id = 'b']")
    , (".a#b.c", "*[contains(concat(' ', normalize-space(./@class), ' '), ' a ') and ./@id = 'b' and contains(concat(' ', normalize-space(./@class), ' '), ' c ')]")

      -- unions
    , ("a, b", "a | b")

      -- attrs
    , ("a[b]",       "a[./@b]")
    , ("a[b=c]",     "a[./@b = 'c']")
    , ("a[b=\"c\"]", "a[./@b = 'c']")
    , ("a[b='c']",   "a[./@b = 'c']")
    , ("a[b*='c']",  "a[contains(./@b, 'c')]")
    , ("a[b!='c']",  "a[not(contains(./@b, 'c'))]")
    , ("a[b~='c']",  "a[contains(concat(' ', normalize-space(./@b), ' '), ' c ')]")
    , ("a[b|='c']",  "a[starts-with(./@b, 'c-')]")
    , ("a[b^='c']",  "a[starts-with(./@b, 'c')]")
    , ("a[b$='c']",  "a[ends-with(./@b, 'c')]")

      -- string escaping
    , ("a[b=\"c \\\"d\\\" e\"]", "a[./@b = 'c \"d\" e']")
    , ("a[b='c \\'d\\' e']",     "a[./@b = concat('c ',\"'\",'d',\"'\",' e')]")

      -- pseudo classes
    , ("a:first-child",   "a[count(preceding-sibling::*) = 0]")
    , ("a:first-of-type", "a[position() = 1]")
    , ("a:last-of-type",  "a[position() = last()]")

      -- pseudo funcs
    , ("a:not(b)", "a[not(b)]")
    , ("a:has(b)", "a[b]")

    , ("a:nth-child(3)",      "a[count(preceding-sibling::*) = 2]")
    , ("a:nth-last-child(3)", "a[count(following-sibling::*) = 2]")
    , ("a:nth-of-type(odd)",  "a[(position() mod 2) = 1]")
    , ("a:nth-of-type(even)", "a[(position() mod 2) = 0]")
    , ("a:nth-of-type(1n+2)", "a[(position() >= 2) and (((position() - 2) mod 1) = 0)]")
    , ("a:nth-of-type(3)",    "a[(position() >= 3) and (((position() - 3) mod 1) = 0)]")
    , ("a:nth-of-type(2n)",   "a[(position() mod 2) = 0]")

    , ("a:nth-last-of-type(odd)",  "a[(position() mod 2) = 1]")
    , ("a:nth-last-of-type(even)", "a[(position() mod 2) = 0]")
    , ("a:nth-last-of-type(1n+2)", "a[((last() - position() + 1) >= 2) and ((((last() - position() + 1) - 2) mod 1) = 0)]")
    , ("a:nth-last-of-type(3)",    "a[((last() - position() + 1) >= 3) and ((((last() - position() + 1) - 3) mod 1) = 0)]")
    , ("a:nth-last-of-type(2n)",   "a[((last() - position() + 1) mod 2) = 0]")

    , ("a:nth-last-child(3)", "a[count(following-sibling::*) = 2]")
    , ("a:nth-child(3)",      "a[count(preceding-sibling::*) = 2]")

      -- combinators
    , ("a b",   "a//b")
    , ("a > b", "a/b")
    , ("a + b", "a/following-sibling::*[1]/self::b")
    , ("a ~ b", "a/preceding-sibling::*[1]/self::b")

      -- multiple combinators
    , ("a b c", "a//b//c")
    ]
