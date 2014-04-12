module Happybara.WebKit.Server where

import           System.Info                 (os)

import           Paths_happybara_webkit_server      (getDataFileName)

webkitServerPath :: IO FilePath
webkitServerPath = getDataFileName $ if os == "mingw32"
                                        then "webkit_server.exe"
                                        else "webkit_server"
