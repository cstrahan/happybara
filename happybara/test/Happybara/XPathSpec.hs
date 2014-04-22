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

examples = [ ] -- sigh.
