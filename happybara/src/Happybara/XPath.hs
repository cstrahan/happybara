{-# LANGUAGE OverloadedStrings #-}

-- This module is generated from the Ruby 'xpath' gem.
-- See: https://gist.github.com/cstrahan/10015991

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
--
-- This module provides XPath constructors for common HTML queries.
-- The 'Text' argument is the locator (e.g. id, type, href, etc), and the 'Bool'
-- argument indicates whether the generated XPath should match exactly or
-- inexactly.
--
-- XPath string literals can be properly quoted and escaped using
-- 'stringLiteral'.
--
-- /Note:/ These functions are mostly meant for internal use;
-- you probably want to use the queries in "Happybara.Query".
module Happybara.XPath where

import           Data.Maybe    (maybe)
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Happybara.CSS

normalizeSpace :: Text -> Text
normalizeSpace = T.unwords . T.words

stringLiteral :: Text -> Text
stringLiteral str =
    go $ T.splitOn "'" str
  where
    go (x:[]) = T.concat ["'", x, "'"]
    go (xs)   = T.concat ["concat('", T.intercalate "',\"'\",'" xs, "')"]

fromCSS :: [Selector] -> Text
fromCSS sels =
    T.intercalate " | " $ map renderSelector sels
  where
    renderElem elem = maybe "*" id elem

    renderSelector (SimpleSelector elem constraints) = T.concat $ [ renderElem elem, renderConstraints constraints ]
    renderSelector (Descendent l r) = T.concat [ renderSelector l, "//", renderSelector r ]
    renderSelector (ImmediateChild l r) = T.concat [ renderSelector l, "/", renderSelector r ]
    renderSelector (AdjacentSibling l r) = T.concat [ renderSelector l, "/following-sibling::*[1]/self::", renderSelector r ]
    renderSelector (PreviousSibling l r) = T.concat [ renderSelector l, "/preceding-sibling::*[1]/self::", renderSelector r ]

    renderConstraints [] = ""
    renderConstraints cs = T.concat [ "[", T.intercalate " and " $ map renderConstraint cs, "]" ]

    renderConstraint :: Constraint -> Text
    renderConstraint (Class klass) = T.concat [ "contains(concat(' ', normalize-space(/@class), ' '), '", klass, "')" ]
    renderConstraint (ID i) = T.concat [ "/@id = ", stringLiteral i ]
    renderConstraint (HasAttribute attr) = T.concat [ "./@", attr ]
    renderConstraint (AttributeEquals attr val) = T.concat [ "./@", attr, " = ", renderStr val ]
    renderConstraint (AttributeContains attr val) = T.concat [ "contains(./@", attr, ", ", renderStr val, ")" ]
    renderConstraint (AttributeDoesNotContain attr val) = T.concat [ "not(", renderConstraint $ AttributeContains attr val, ")"]
    renderConstraint (AttributeContainsWord attr val) = T.concat [ "contains(concat(' ', normalize-space(./@", attr, "), ' '), '", (renderStr' " " val " "), "')" ]
    renderConstraint (AttributeContainsPrefix attr val) = T.concat [ "starts-with(./@", attr, ", ", renderStr  val, ")" ]
    renderConstraint (AttributeStartsWith attr val) = T.concat [ "starts-with(./@", attr, ", ", renderStr  val, ")" ]
    renderConstraint (AttributeEndsWith attr val) = T.concat [ "ends-with(./@", attr, ", ", renderStr val, ")" ]

    renderConstraint (PseudoFunc "not" (SelectorArg sel)) = T.concat [ "not(", renderSelector sel, ")" ]

    renderConstraint (PseudoFunc "has" (SelectorArg sel)) = renderSelector sel

    renderConstraint (PseudoFunc "nth-child" (NPlusBArg b)) = T.concat [ "count(preceding-sibling::*) = ", int2txt (b-1) ]
    renderConstraint (PseudoFunc "nth-last-child" (NPlusBArg b)) = T.concat [ "count(following-sibling::*) = ", int2txt (b-1) ]

    renderConstraint (PseudoFunc "nth-of-type" OddArg) = "(position() mod 2) = 1"
    renderConstraint (PseudoFunc "nth-of-type" EvenArg) = "(position() mod 2) = 0"
    renderConstraint (PseudoFunc "nth-of-type" (ANPlusBArg a b)) = nthOfTypeMod a b
    renderConstraint (PseudoFunc "nth-of-type" (NPlusBArg b)) = nthOfTypeMod 1 b
    renderConstraint (PseudoFunc "nth-of-type" (ANArg a)) = nthOfTypeMod a 0

    renderConstraint (PseudoFunc "nth-last-of-type" OddArg) = "(position() mod 2) = 1"
    renderConstraint (PseudoFunc "nth-last-of-type" EvenArg) = "(position() mod 2) = 0"
    renderConstraint (PseudoFunc "nth-last-of-type" (ANPlusBArg a b)) = nthOfTypeMod a b
    renderConstraint (PseudoFunc "nth-last-of-type" (NPlusBArg b)) = nthOfTypeMod 1 b
    renderConstraint (PseudoFunc "nth-last-of-type" (ANArg a)) = nthOfTypeMod a 0

    renderConstraint (PseudoClass "first-of-type") = "position() = 1"
    renderConstraint (PseudoClass "last-of-type") = "position() = 1"
    renderConstraint (PseudoClass "first-child" ) = "count(preceding-sibling::*) = 0";
    renderConstraint (PseudoClass "last-child" ) = "count(following-sibling::*) = 0";
    renderConstraint (PseudoClass "only-child" ) = "count(preceding-sibling::*) = 0 and count(following-sibling::*) = 0";
    renderConstraint (PseudoClass "only-of-type" ) = "last() = 1";
    renderConstraint (PseudoClass "empty" ) = "not(node())";

    nthOfTypeMod a b
        | a == (-1) = T.concat [ "(position() <= ", int2txt b, ") and (((position() - ", int2txt b, ") mod 1) = 0)" ]
        | b > 0     = T.concat [ "(position() >= ", int2txt b, ") and (((position() - ", int2txt b, ") mod ", int2txt a, ") = 0)" ]
        | otherwise = T.concat [ "(position() mod ", int2txt a, ") = 0" ]

    nthLastOfTypeMod a b
        | a == (-1) = T.concat [ "((last() - position() + 1) <= ", int2txt b, ") and ((((last() - position() + 1) - ", int2txt b, ") mod 1) = 0)" ]
        | b > 0     = T.concat [ "((last() - position() + 1) >= ", int2txt b, ") and ((((last() - position() + 1) - ", int2txt b, ") mod ", int2txt a, ") = 0)" ]
        | otherwise = T.concat [ "((last() - position() + 1) mod ", int2txt a, ") = 0" ]

    int2txt i = T.pack $ show (i::Int)
    identOrStringToString (Ident i) = i
    identOrStringToString (StringLit lit) = lit
    renderStr    x   = stringLiteral $ identOrStringToString x
    renderStr' l x r = stringLiteral $ T.concat [ l, identOrStringToString x, r ]

link :: Text -> Bool -> Text
link locator exact =
    T.concat $ if exact
                 then [".//a[./@href][(((./@id = ", locatorLiteral, " or normalize-space(string(.)) = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ") or .//img[./@alt = ", locatorLiteral, "])]"]
                 else [".//a[./@href][(((./@id = ", locatorLiteral, " or contains(normalize-space(string(.)), ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, ")) or .//img[contains(./@alt, ", locatorLiteral, ")])]"]
  where
    locatorLiteral = stringLiteral locator

button :: Text -> Bool -> Text
button locator exact =
    T.concat $ if exact
                 then [".//input[./@type = 'submit' or ./@type = 'reset' or ./@type = 'image' or ./@type = 'button'][((./@id = ", locatorLiteral, " or ./@value = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ")] | .//input[./@type = 'image'][./@alt = ", locatorLiteral, "] | .//button[(((./@id = ", locatorLiteral, " or ./@value = ", locatorLiteral, ") or normalize-space(string(.)) = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ")] | .//input[./@type = 'image'][./@alt = ", locatorLiteral, "]"]
                 else [".//input[./@type = 'submit' or ./@type = 'reset' or ./@type = 'image' or ./@type = 'button'][((./@id = ", locatorLiteral, " or contains(./@value, ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, "))] | .//input[./@type = 'image'][contains(./@alt, ", locatorLiteral, ")] | .//button[(((./@id = ", locatorLiteral, " or contains(./@value, ", locatorLiteral, ")) or contains(normalize-space(string(.)), ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, "))] | .//input[./@type = 'image'][contains(./@alt, ", locatorLiteral, ")]"]
  where
    locatorLiteral = stringLiteral locator

linkOrButton :: Text -> Bool -> Text
linkOrButton locator exact =
    T.concat $ if exact
                 then [".//a[./@href][(((./@id = ", locatorLiteral, " or normalize-space(string(.)) = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ") or .//img[./@alt = ", locatorLiteral, "])] | .//input[./@type = 'submit' or ./@type = 'reset' or ./@type = 'image' or ./@type = 'button'][((./@id = ", locatorLiteral, " or ./@value = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ")] | .//input[./@type = 'image'][./@alt = ", locatorLiteral, "] | .//button[(((./@id = ", locatorLiteral, " or ./@value = ", locatorLiteral, ") or normalize-space(string(.)) = ", locatorLiteral, ") or ./@title = ", locatorLiteral, ")] | .//input[./@type = 'image'][./@alt = ", locatorLiteral, "]"]
                 else [".//a[./@href][(((./@id = ", locatorLiteral, " or contains(normalize-space(string(.)), ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, ")) or .//img[contains(./@alt, ", locatorLiteral, ")])] | .//input[./@type = 'submit' or ./@type = 'reset' or ./@type = 'image' or ./@type = 'button'][((./@id = ", locatorLiteral, " or contains(./@value, ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, "))] | .//input[./@type = 'image'][contains(./@alt, ", locatorLiteral, ")] | .//button[(((./@id = ", locatorLiteral, " or contains(./@value, ", locatorLiteral, ")) or contains(normalize-space(string(.)), ", locatorLiteral, ")) or contains(./@title, ", locatorLiteral, "))] | .//input[./@type = 'image'][contains(./@alt, ", locatorLiteral, ")]"]
  where
    locatorLiteral = stringLiteral locator

fieldset :: Text -> Bool -> Text
fieldset locator exact =
    T.concat $ if exact
                 then [".//fieldset[(./@id = ", locatorLiteral, " or ./legend[normalize-space(string(.)) = ", locatorLiteral, "])]"]
                 else [".//fieldset[(./@id = ", locatorLiteral, " or ./legend[contains(normalize-space(string(.)), ", locatorLiteral, ")])]"]
  where
    locatorLiteral = stringLiteral locator

field :: Text -> Bool -> Text
field locator exact =
    T.concat $ if exact
                 then [".//*[self::input | self::textarea | self::select][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'hidden')][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//*[self::input | self::textarea | self::select][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'hidden')]"]
                 else [".//*[self::input | self::textarea | self::select][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'hidden')][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//*[self::input | self::textarea | self::select][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'hidden')]"]
  where
    locatorLiteral = stringLiteral locator

fillableField :: Text -> Bool -> Text
fillableField locator exact =
    T.concat $ if exact
                 then [".//*[self::input | self::textarea][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'radio' or ./@type = 'checkbox' or ./@type = 'hidden' or ./@type = 'file')][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//*[self::input | self::textarea][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'radio' or ./@type = 'checkbox' or ./@type = 'hidden' or ./@type = 'file')]"]
                 else [".//*[self::input | self::textarea][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'radio' or ./@type = 'checkbox' or ./@type = 'hidden' or ./@type = 'file')][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//*[self::input | self::textarea][not(./@type = 'submit' or ./@type = 'image' or ./@type = 'radio' or ./@type = 'checkbox' or ./@type = 'hidden' or ./@type = 'file')]"]
  where
    locatorLiteral = stringLiteral locator

select :: Text -> Bool -> Text
select locator exact =
    T.concat $ if exact
                 then [".//select[(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//select"]
                 else [".//select[(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//select"]
  where
    locatorLiteral = stringLiteral locator

checkbox :: Text -> Bool -> Text
checkbox locator exact =
    T.concat $ if exact
                 then [".//input[./@type = 'checkbox'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//input[./@type = 'checkbox']"]
                 else [".//input[./@type = 'checkbox'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//input[./@type = 'checkbox']"]
  where
    locatorLiteral = stringLiteral locator

radioButton :: Text -> Bool -> Text
radioButton locator exact =
    T.concat $ if exact
                 then [".//input[./@type = 'radio'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//input[./@type = 'radio']"]
                 else [".//input[./@type = 'radio'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//input[./@type = 'radio']"]
  where
    locatorLiteral = stringLiteral locator

fileField :: Text -> Bool -> Text
fileField locator exact =
    T.concat $ if exact
                 then [".//input[./@type = 'file'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[normalize-space(string(.)) = ", locatorLiteral, "]/@for)] | .//label[normalize-space(string(.)) = ", locatorLiteral, "]//.//input[./@type = 'file']"]
                 else [".//input[./@type = 'file'][(((./@id = ", locatorLiteral, " or ./@name = ", locatorLiteral, ") or ./@placeholder = ", locatorLiteral, ") or ./@id = //label[contains(normalize-space(string(.)), ", locatorLiteral, ")]/@for)] | .//label[contains(normalize-space(string(.)), ", locatorLiteral, ")]//.//input[./@type = 'file']"]
  where
    locatorLiteral = stringLiteral locator

optgroup :: Text -> Bool -> Text
optgroup locator exact =
    T.concat $ if exact
                 then [".//optgroup[./@label = ", locatorLiteral, "]"]
                 else [".//optgroup[contains(./@label, ", locatorLiteral, ")]"]
  where
    locatorLiteral = stringLiteral locator

option :: Text -> Bool -> Text
option locator exact =
    T.concat $ if exact
                 then [".//option[normalize-space(string(.)) = ", locatorLiteral, "]"]
                 else [".//option[contains(normalize-space(string(.)), ", locatorLiteral, ")]"]
  where
    locatorLiteral = stringLiteral locator

table :: Text -> Bool -> Text
table locator exact =
    T.concat $ if exact
                 then [".//table[(./@id = ", locatorLiteral, " or .//caption = ", locatorLiteral, ")]"]
                 else [".//table[(./@id = ", locatorLiteral, " or contains(.//caption, ", locatorLiteral, "))]"]
  where
    locatorLiteral = stringLiteral locator

definitionDescription :: Text -> Text
definitionDescription locator =
    T.concat [".//dd[(./@id = ", locatorLiteral, " or ./preceding-sibling::*[1]/self::dt[normalize-space(string(.)) = ", locatorLiteral, "])]"]
  where
    locatorLiteral = stringLiteral locator
