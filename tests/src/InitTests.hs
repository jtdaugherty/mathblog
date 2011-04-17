module InitTests where

-- import System.Unix.Directory
--     ( withTemporaryDirectory
--     )
import Test.Framework
    ( testGroup
    , Test)
import Test.Framework.Providers.HUnit
import Test.HUnit
    ( (@=?) )

tests :: Test
tests = testGroup "blog initialization tests" [
        ]

-- testInitBlog :: Test
-- testInitBlog = withTemporaryDirectory "mb-temp" runTest