module Main where


import qualified InitTests as InitTests

import Test.Framework ( defaultMain )

main :: IO ()
main = defaultMain [ InitTests.tests
                   ]