{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import           Happybara.Classes        (Driver, Happybara, HappybaraT,
                                           NodeValue (..), findOrFail)
import           Happybara.Monad
import           Happybara.Query
import           Happybara.WebKit.Driver
import           Happybara.WebKit.Session
import qualified Happybara.XPath          as XPath

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.State

import qualified Data.ByteString.Char8    as BS
import           Data.Text                as T
import           Data.Text.Encoding       as T

import qualified System.IO                as IO

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
    serverPath <- defaultServerPath
    withSession serverPath $ \sess -> do
        runHappybara sess act

puts :: Text -> Happybara sess ()
puts txt = do
    liftIO $ BS.hPutStrLn IO.stdout $ T.encodeUtf8 txt
