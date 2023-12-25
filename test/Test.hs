module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import DecodeTest qualified
import JsonTest qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All tests"
        [ JsonTest.test
        , DecodeTest.test
        ]
