module DecodeTest (test) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Decode

test :: TestTree
test =
    testGroup
        "Decoder tests"
        [ decodeDouble
        , decodeInt
        ]

decodeDouble :: TestTree
decodeDouble =
    testGroup
        "double"
        [ testCase "3.14" $ decodeJson double "3.14" @?= Right 3.14
        , testCase "314" $ decodeJson double "314" @?= Right 314
        , testCase "-42.0" $ decodeJson double "-42.0" @?= Right (-42.0)
        , testCase "true" $ decodeJson double "true" @?= Left "true is not a double"
        ]

decodeInt :: TestTree
decodeInt =
    testGroup
        "int"
        [ testCase "42" $ decodeJson int "42" @?= Right 42
        , testCase "42.0" $ decodeJson int "42.0" @?= Right 42
        , testCase "3.14" $ decodeJson int "3.14" @?= Left "3.14 is not an integer"
        , testCase "true" $ decodeJson int "true" @?= Left "true is not an integer"
        ]
