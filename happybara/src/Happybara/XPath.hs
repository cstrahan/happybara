{-# LANGUAGE OverloadedStrings #-}

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

import           Data.Maybe       (maybe)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Text.Parsec.Prim (runParser)

import           Happybara.CSS

normalizeSpace :: Text -> Text
normalizeSpace = T.unwords . T.words

stringLiteral :: Text -> Text
stringLiteral str =
    go $ T.splitOn "'" str
  where
    go (x:[]) = T.concat ["'", x, "'"]
    go (xs)   = T.concat ["concat('", T.intercalate "',\"'\",'" xs, "')"]

fromCSS :: Text -> Text -> Either Text Text
fromCSS prefix css = do
    ast <- parse css
    sels <- mapM (renderSelector) ast
    return $ if T.null prefix
               then T.intercalate " | " sels
               else prefix <> T.intercalate (" | "<>prefix) sels
  where
    parse = fromEither . parseCSS
      where
        fromEither (Right x) = Right x
        fromEither (Left msg) = Left $ T.concat [ "css parse error: ", T.pack $ show msg ]

    renderElem elem = maybe "*" id elem

    renderSelector (SimpleSelector elem constraints) = do
        constraints' <- renderConstraints constraints
        return $ T.concat [ renderElem elem, constraints' ]
    renderSelector (Descendent l r) = do
        r' <- renderSelector r
        l' <- renderSelector l
        return $ T.concat [ l', "//", r' ]
    renderSelector (ImmediateChild l r) = do
        r' <- renderSelector r
        l' <- renderSelector l
        return $ T.concat [ l', "/", r' ]
    renderSelector (AdjacentSibling l r) = do
        r' <- renderSelector r
        l' <- renderSelector l
        return $ T.concat [ l', "/following-sibling::*[1]/self::", r' ]
    renderSelector (PreviousSibling l r) = do
        r' <- renderSelector r
        l' <- renderSelector l
        return $ T.concat [ l', "/preceding-sibling::*[1]/self::", r' ]

    renderConstraints [] = return ""
    renderConstraints cs = do
        cs' <- mapM renderConstraint cs
        return $ T.concat [ "[", T.intercalate " and " $ cs', "]" ]

    renderConstraint (Class klass) = return $ T.concat [ "contains(concat(' ', normalize-space(./@class), ' '), ' ", klass, " ')" ]
    renderConstraint (ID i) = return $ T.concat [ "./@id = ", stringLiteral i ]
    renderConstraint (HasAttribute attr) = return $ T.concat [ "./@", attr ]
    renderConstraint (AttributeEquals attr val) = return $ T.concat [ "./@", attr, " = ", renderStr val ]
    renderConstraint (AttributeContains attr val) = return $ T.concat [ "contains(./@", attr, ", ", renderStr val, ")" ]
    renderConstraint (AttributeDoesNotContain attr val) = return $ T.concat [ "not(contains(./@", attr, ", ", renderStr val, "))" ]
    renderConstraint (AttributeContainsWord attr val) = return $ T.concat [ "contains(concat(' ', normalize-space(./@", attr, "), ' '), ", renderStr' " " val " ", ")" ]
    renderConstraint (AttributeContainsPrefix attr val) = return $ T.concat [ "./@", attr, " = ", renderStr val, " or starts-with(./@", attr, ", ", renderStr' "" val "-", ")" ]
    renderConstraint (AttributeStartsWith attr val) = return $ T.concat [ "starts-with(./@", attr, ", ", renderStr val, ")" ]
    renderConstraint (AttributeEndsWith attr val) = return $ T.concat [ "ends-with(./@", attr, ", ", renderStr val, ")" ]

    renderConstraint (PseudoFunc "not" (SelectorArg sel)) = do
        sel' <- renderSelector sel
        return $ T.concat [ "not(", sel', ")" ]
    renderConstraint (PseudoFunc "not" _) = Left "invalid argument to :not"

    renderConstraint (PseudoFunc "has" (SelectorArg sel)) = renderSelector sel
    renderConstraint (PseudoFunc "has" _) = Left "invalid argument to :has"

    renderConstraint (PseudoFunc "nth-child" (ANPlusBArg a b)) = return $ nthChild a b
    renderConstraint (PseudoFunc "nth-child" _) = Left "invalid argument to :nth-child"

    renderConstraint (PseudoFunc "nth-last-child" (ANPlusBArg a b)) = return $ nthLastChild a b
    renderConstraint (PseudoFunc "nth-last-child" _) = Left "invalid argument to :nth-last-child"

    renderConstraint (PseudoFunc "nth-of-type" (ANPlusBArg a b)) = return $ nthOfType a b
    renderConstraint (PseudoFunc "nth-of-type" _) = Left "invalid argument to :nth-of-type"

    renderConstraint (PseudoFunc "nth-last-of-type" (ANPlusBArg a b)) = return $ nthLastOfType a b
    renderConstraint (PseudoFunc "nth-last-of-type" _) = Left "invalid argument to :nth-last-of-type"
    renderConstraint (PseudoFunc sel _) = Left $ T.concat [ "unknown pseudo func :", sel ]

    renderConstraint (PseudoClass "first-of-type") = return "position() = 1"
    renderConstraint (PseudoClass "last-of-type") = return "position() = last()"
    renderConstraint (PseudoClass "first-child") = return "count(preceding-sibling::*) = 0"
    renderConstraint (PseudoClass "last-child") = return "count(following-sibling::*) = 0"
    renderConstraint (PseudoClass "only-child") = return "count(preceding-sibling::*) = 0 and count(following-sibling::*) = 0"
    renderConstraint (PseudoClass "only-of-type") = return "last() = 1"
    renderConstraint (PseudoClass "empty") = return "not(node())"
    renderConstraint (PseudoClass "link") = return "local-name()='a' and ./@href"
    renderConstraint (PseudoClass "disabled") = return "(local-name()='textarea' or local-name()='input') and ./@disabled"
    renderConstraint (PseudoClass "checked") = return "./@checked and (local-name()='option' or (local-name()='input' and (./@type='radio' or ./@type='checkbox'))) "
    renderConstraint (PseudoClass "selected") = return "./@selected"
    renderConstraint (PseudoClass sel) = Left $ T.concat [ "unknown pseudo class :", sel ]

    nthChild a b
        | a < 0     = T.concat [ "(count(preceding-sibling::*) ", addOrSubtract (-b-1), ") mod ", int2txt (abs a), " = 0 and count(preceding-sibling::*) <= ", int2txt (b-1) ]
        | a == 0    = T.concat [ "(count(preceding-sibling::*)) = ", int2txt (b-1) ]
        | otherwise = T.concat [ "(count(preceding-sibling::*) ", addOrSubtract (-b-1), ") mod ", int2txt a, " = 0" ]

    nthLastChild a b
        | a < 0     = T.concat [ "(count(following-sibling::*) ", addOrSubtract (-b-1), ") mod ", int2txt (abs a), " = 0 and count(following-sibling::*) <= ", int2txt (b-1) ]
        | a == 0    = T.concat [ "(count(following-sibling::*)) = ", int2txt (b-1) ]
        | otherwise = T.concat [ "(count(following-sibling::*) ", addOrSubtract (-b-1), ") mod ", int2txt a, " = 0" ]

    nthOfType a b
        | a < 0     = T.concat [ "(position() ", addOrSubtract (-b), ") mod ", int2txt (abs a), " = 0 and position() <= ", int2txt b ]
        | a == 0    = T.concat [ "position() = ", int2txt b ]
        | otherwise = T.concat [ "(position() ", addOrSubtract (-b), ") mod ", int2txt a, " = 0" ]

    nthLastOfType a b
        | a < 0     = T.concat [ "(last() - position() ", addOrSubtract (-b-1), ") mod ", int2txt (abs a), " = 0 and (last() - position()) <= ", int2txt (b-1) ]
        | a == 0    = T.concat [ "(last() - position()) = ", int2txt (b-1) ]
        | otherwise = T.concat [ "(last() - position() ", addOrSubtract (-b-1), ") mod ", int2txt a, " = 0" ]

    addOrSubtract n
        | n < 0     = "- " <> int2txt (abs n)
        | otherwise = "+ " <> int2txt n

    int2txt i = T.pack $ show (i::Int)
    identOrStringToString (Ident i) = i
    identOrStringToString (StringLit lit) = lit
    renderStr    x   = stringLiteral $ identOrStringToString x
    renderStr' l x r = stringLiteral $ T.concat [ l, identOrStringToString x, r ]

-- The following XPaths were generated from the Ruby 'xpath' gem.
-- See: https://gist.github.com/cstrahan/10015991

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
