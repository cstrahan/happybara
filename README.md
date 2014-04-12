# Happybara

Happybara helps you test web applications in Haskell by simulating how a
real user would interact with your app. It is agnostic about the driver
running your tests and comes with with WebKit support built in.

## Drivers

Happybara Drivers implement the `Happybara.Classes.Driver` type-class.
The `happybara-webkit` contains support for WebKit.

## The DSL

**Note**: All searches in Happybara are *case sensitive*. This is
because Happybara heavily uses XPath, which doesn't support case
insensitivity.
