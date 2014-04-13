# Happybara

Happybara helps you test web applications in Haskell by simulating how a
real user would interact with your app. It is agnostic about the driver
running your tests and comes with WebKit support via the
`happybara-webkit` package.

## Diving In

Before we look at the design of Happybara, let's first take a quick look
at a minimal example. We're going to use the following packages:

* `happybara`
* `happybara-webkit` - This contains a `Driver` implementation for WebKit.
* `happybara-webkit-server` - This packages up the `webkit_server`
  binary that we need for `happybara-webkit` (you could also compile it
yourself, but using this package should be a bit more turn-key).

**Note**: Before you install the `happybara-webkit-server` package,
you'll need to install Qt4.8 or greater. If you need help, follow the
[capybara-webkit guide](https://github.com/thoughtbot/capybara-webkit/wiki/Installing-Qt-and-compiling-capybara-webkit).

Alright, let's take a look at a simple example:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import           Happybara
import           Happybara.WebKit
import           Happybara.WebKit.Server

import           Control.Monad.Base

import qualified Data.ByteString.Char8 as BS
import           Data.Text             as T
import           Data.Text.Encoding    as T

import qualified System.IO             as IO

main :: IO ()
main = run $ do
    visit "http://google.com"

    btn <- findOrFail (button "I'm Feeling Lucky" [disabled False])
    SingleValue value <- getValue btn
    puts $ T.concat [ "Button found: ", value ]

    click btn

    url <- currentUrl
    puts $ T.concat [ "New url: ", url ]

    return ()

run :: Happybara Session a -> IO a
run act = do
    serverPath <- webkitServerPath
    withSession serverPath $ \sess ->
        runHappybara sess act

puts :: Text -> Happybara sess ()
puts txt =
    liftBase $ BS.hPutStrLn IO.stdout $ T.encodeUtf8 txt
```

Let's first look at `main`. In this minimal example, we:

* Visit the google home page.
* Find an enabled button with the text "I'm Feeling Lucky".
* Print the value of the button.
* Click the button
* Print the new url.

This is just scratching the surface; to get a broader sense of the API,
you can keep reading this document, or move on to reading the
[Haddocks](hackage.haskell.org/package/happybara).

Let's change our focus to `run` now. The first thing to notice is that
we parameterized our Happybara monad with `Happybara.WebKit.Session`;
this is our way of saying that we wish to use the WebKit Driver. You
see, Happybara has a pluggable driver architecture, and `Session` is
just one (of potentially many) `Driver` instances. You could just as
well use a PhantomJS driver, or Selenium driver.

The rest of the `run` function merely handles starting up the WebKit
server.

Now let's move on to the design and interface of Happybara.

## Drivers

As we've already touched on, Happybara Drivers implement the
`Happybara.Driver.Driver` class.

## The DSL

**Note**: All searches in Happybara are *case sensitive*. This is
because Happybara heavily uses XPath, which doesn't support case
insensitivity.

Happybara features a rich DOM querying system. Queries are performed via
the `Query` class and it's respective functions. As we saw earlier,
`button locator predicates` is one example of a query, where `locator` is
a string denoting an id, value, etc, and `predicates` is a list of
filters.

Queries can be performed via `find`, `findOrFail`, and `findAll`.

## Exactness

When querying the DOM, it's often convenient to search in an inexact
manner (e.g. via substring, rather than strict equality). And yet there
are other times when you know exactly what you want to find, and nothing
else will do.

Happybara allows you to demand a particular level of exactness via
`setExactness`. There are three different settings:

* `Exact` - Find elements that match exactly.
* `PreferExact` - First try to find exact matches; if that fails, fall
  back to inexact matches.
* `Inexact` - Find all elements that partially match - e.g. the given
  string is infix of (but not necessarily equal to) whatever property
(id, attribute, etc) is being queried over.

## Matching Strategy

When a query is performed for a single value, and multiple matches are
found, you need to decide if that should yield the first result, or fail
hard with an `AmbiguousElementException`. Using
`setSingleMatchStrategy`, these are your two options:

* `MatchFirst`
* `MatchOne`

By having the matching strategy as part of the monadic state, queries
compose in convenient, consistent way. For instance, consider the
following:

``` haskell
fillIn (fillableField "User Name" []) "Charles Strahan"
```

If you've set `MatchFirst`, the code above will fill in the first
matching field; if you set `MatchOne`, you'll get an exception if more
than one field matched.
