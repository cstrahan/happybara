import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Configure
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Verbosity

import           System.Directory
import           System.Environment
import           System.Exit
import           System.Info
import           System.IO
import           System.IO.Unsafe
import           System.Process                     hiding (env)

import           Control.Applicative
import           Control.Monad

import           Data.List                          (foldl', partition,
                                                     stripPrefix)
import           Data.Maybe                         (mapMaybe, maybe)

sh :: String
   -> [String]
   -> IO ()
sh cmd args = do
    putStrLn $ ">>> " ++ cmd ++ " " ++ unwords (map show args)
    exitCode <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
    unless (exitCode == ExitSuccess) $
        fail $ "Command \"" ++ cmd ++ "\" failed."

env :: [(String, String)]
env = unsafePerformIO getEnvironment

envOr :: String -> String -> String
envOr k d = maybe d id (lookup k env)

make_bin = envOr "MAKE" "make"

qmake_bin = envOr "QMAKE" default_qmake_binary

spec = envOr "SPEC" os_spec

default_qmake_binary =
    if os == "freebsd"
      then "qmake-qt4"
      else "qmake"

os_spec | os == "linux"   = "linux-g++"
        | os == "freebsd" = "freebsd-g++"
        | os == "mingw32" = "win32-g++"
        | os == "darwin"  = "macx-g++"

makefile = do
    sh qmake_bin ["-spec", spec, ""]

qmake = do
    sh make_bin ["qmake"]

path_to_binary =
    if os == "mingw32"
      then "src/debug/webkit_server.exe"
      else "src/webkit_server"

build_all = do
    setCurrentDirectory "capybara-webkit"

    makefile
    qmake
    sh make_bin []
    createDirectoryIfMissing False "../data"
    copyFile path_to_binary "../data/webkit_server"

    setCurrentDirectory ".."

customBuildHook pkg_descr localbuildinfo hooks flags = do
    putStrLn "Building webkit_server"
    build_all
    (buildHook simpleUserHooks) pkg_descr localbuildinfo hooks flags

-- add the server binary to the list of data files
customConfHook :: (GenericPackageDescription, HookedBuildInfo)
               -> ConfigFlags
               -> IO LocalBuildInfo
customConfHook (pkg_descr0, pbi) cfg = do
    let serverBin = if os == "mingw32"
                      then "webkit_server.exe"
                      else "webkit_server"
    let pkg_descr = pkg_descr0 {
            packageDescription = (packageDescription pkg_descr0) {
                dataFiles = serverBin:(dataFiles . packageDescription $ pkg_descr0)
            }
        }
    (confHook simpleUserHooks) (pkg_descr, pbi) cfg

main :: IO ()
main = do
    defaultMainWithHooks $ simpleUserHooks
        { confHook  = customConfHook
        , buildHook = customBuildHook
        }
