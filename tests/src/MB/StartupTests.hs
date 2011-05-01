module MB.StartupTests where

import Test.Framework
    ( testGroup
    , Test)
import Test.Framework.Providers.HUnit
import Test.HUnit
    ( (@=?), assertFailure )

import MB.Startup

tests :: Test
tests = testGroup "Startup configuration tests" [
         testGroup "Base directory tests" $ map testBaseDir [
                      -- (desc, cmdline, env, base dir result)
                         ("base dir in environment only"
                         , [], [(baseDirEnvName, "foo")], "foo")
                       , ("base dir on command line only"
                         , ["--" ++ baseDirParamName, "foo"], [], "foo")
                       , ("base dir in both places, command line takes precedence"
                         , ["--" ++ baseDirParamName, "foo"], [(baseDirEnvName, "bar")], "foo")
                       ]
        , testCase "No base directory" $ Nothing @=? startupConfig [] []
        ]

testBaseDir :: (String, [String], [(String, String)], FilePath) -> Test
testBaseDir (desc, args, env, result) =
    testCase desc assert
        where mConfig = startupConfig args env
              assert = case mConfig of
                         Nothing -> assertFailure "could not construct configuration"
                         Just cfg -> result @=? dataDirectory cfg


