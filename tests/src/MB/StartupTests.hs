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
         testGroup "Data directory tests" $ map (testBaseDir dataDirectory) [
                      -- (desc, cmdline, env, base dir result)
                         ("base dir in environment only"
                         , [], [ (htmlOutputDirEnvName, "pth")
                               , (baseDirEnvName, "foo")
                               ]
                         , "foo")
                       , ("base dir on command line only"
                         , ["--" ++ baseDirParamName, "foo"]
                         , [(htmlOutputDirEnvName, "pth")]
                         , "foo")
                       , ("base dir in both places, command line takes precedence"
                         , ["--" ++ baseDirParamName, "foo"]
                         , [(baseDirEnvName, "bar"), (htmlOutputDirEnvName, "pth")]
                         , "foo")
                       ]
        , testGroup "HTML output directory tests" $ map (testBaseDir htmlOutputDirectory) [
                      -- (desc, cmdline, env, base dir result)
                         ("html dir in environment only"
                         , [], [ (htmlOutputDirEnvName, "pth")
                               , (baseDirEnvName, "foo")
                               ]
                         , "pth")
                       , ("html dir on command line only"
                         , ["--" ++ htmlDirParamName, "pth"]
                         , [(baseDirEnvName, "foo")]
                         , "pth")
                       , ("html dir in both places, command line takes precedence"
                         , ["--" ++ htmlDirParamName, "pth1"]
                         , [(baseDirEnvName, "bar"), (htmlOutputDirEnvName, "pth2")]
                         , "pth1")
                       ]
        , testGroup "No base directory specified" $
                        [ testCase "HTML output dir in environment" $ Nothing @=? startupConfig [] [(htmlOutputDirEnvName, "foo")]
                        , testCase "HTML output dir in args" $ Nothing @=? startupConfig ["--" ++ htmlDirParamName, "foo"] []
                        ]
        , testGroup "No html output directory specified" $
                    [ testCase "Data dir in environment" $ Nothing @=? startupConfig [] [(baseDirEnvName, "foo")]
                    , testCase "Data dir in args" $ Nothing @=? startupConfig ["--" ++ baseDirParamName, "foo"] []
                    ]
        ]

testBaseDir :: (Show a, Eq a) => (StartupConfig -> a) -> (String, [String], [(String, String)], a) -> Test
testBaseDir f (desc, args, env, result) =
    testCase desc assert
        where mConfig = startupConfig args env
              assert = case mConfig of
                         Nothing -> assertFailure "could not construct configuration"
                         Just cfg -> result @=? f cfg
