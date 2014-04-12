{-# LANGUAGE OverloadedStrings #-}

-- This module is generated from the Ruby 'xpath' gem.
-- See: https://gist.github.com/cstrahan/10015991

module Happybara.XPath where
import           Data.Text as T

stringLiteral :: Text -> Text
stringLiteral str =
    go $ splitOn "'" str
  where
    go (x:[]) = T.concat ["'", x, "'"]
    go (xs)   = T.concat ["concat('", T.intercalate "',\"'\",'" xs, "')"]

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
