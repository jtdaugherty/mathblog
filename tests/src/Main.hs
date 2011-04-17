module Main where


import qualified MB.ParamTests as ParamTests
import qualified InitTests as InitTests

import Test.Framework ( defaultMain )

main :: IO ()
main = defaultMain [ ParamTests.tests
                   , InitTests.tests
                   ]