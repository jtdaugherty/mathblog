module Main where

import qualified MB.StartupTests as StartupTests
import qualified InitTests as InitTests

import Test.Framework ( defaultMain )

main :: IO ()
main = defaultMain [ StartupTests.tests
                   , InitTests.tests
                   ]