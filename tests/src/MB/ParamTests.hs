module MB.ParamTests where

import Test.Framework
    ( testGroup
    , Test)
import Test.Framework.Providers.HUnit
import Test.HUnit
    ( (@=?) )

import MB.Params

tests :: Test
tests = testGroup "Param parsing tests" [
         testGroup "argValue tests" $ map testArgValue [
                      -- (arg1,      arg2,                      result)
                         ("foo",    ["-v", "--foo=bar"],        Just "bar")
                       , ("--foo",  ["-v", "--foo=bar"],        Nothing)
                       , ("--foo=", ["-v", "--foo=bar"],        Nothing)
                       , ("foo",    ["--foo=baz", "--foo=bar"], Just "baz")
                       , ("v",      ["-v", "--foo=bar"],        Nothing)
                       , ("-v",     ["-v", "--foo=bar"],        Nothing)

                       -- The next tests exhibit arguably confusing behavior:

                       -- Nothing? Just "bar"?
                       , ("foo", ["--foo=", "--foo=bar"], Just "")
                       -- Nothing?
                       , ("v", ["--v=", "--foo=bar"], Just "")

                       -- check for case sensitivity
                       , ("foo", ["-v", "--Foo=bar"], Nothing)
                       , ("Foo", ["-v", "--Foo=bar"], Just "bar")
                       ]
        , testGroup "findBaseDir tests" $ map testFindBaseDir [
                       -- (test description, arg1, arg2, result):
                          ("all empty", [], [], Nothing)
                        , ("just args", ["--baseDir=foo"], [], Just "foo")
                        , ("just env",  [], [(baseDirEnvName, "foo")], Just "foo")
                        , ("both, simple", ["--baseDir=foo"],
                            [(baseDirEnvName, "foo")],  Just "foo")
                        ]
        ]

testArgValue :: (String, [String], Maybe String) -> Test
testArgValue (key, args, result) =
    testCase ("argValue "++key++" "++(show args)) assert
        where assert = result @=? argValue key args

testFindBaseDir :: (String, [String], [(String, String)], Maybe FilePath) -> Test
testFindBaseDir (descr, args, env, result) = testCase descr assert
    where assert = result @=? findBaseDir args env

