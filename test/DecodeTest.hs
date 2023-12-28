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
        , decodeBool
        , decodeString
        , decodeList
        , decodeField
        , decodeIndex
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

decodeBool :: TestTree
decodeBool =
    testGroup
        "bool"
        [ testCase "true" $ decodeJson bool "true" @?= Right True
        , testCase "false" $ decodeJson bool "false" @?= Right False
        , testCase "3.14" $ decodeJson bool "3.14" @?= Left "3.14 is not a boolean"
        ]

decodeString :: TestTree
decodeString =
    testGroup
        "string"
        [ testCase "\"hello\"" $ decodeJson string "\"hello\"" @?= Right "hello"
        , testCase "3.14" $ decodeJson string "3.14" @?= Left "3.14 is not a string"
        ]

decodeList :: TestTree
decodeList =
    testGroup
        "list"
        [ testCase "[1,2,3]" $ decodeJson (list int) "[1,2,3]" @?= Right [1, 2, 3]
        , testCase "[]" $ decodeJson (list int) "[]" @?= Right []
        , testCase "[true,false]" $ decodeJson (list int) "[true,false]" @?= Left "true is not an integer"
        , testCase "42" $ decodeJson (list int) "42" @?= Left "42.0 is not an array"
        ]

decodeField :: TestTree
decodeField =
    testGroup
        "field"
        [ testCase "existing boolean field" $ decodeJson (field "a" bool) "{\"a\":true}" @?= Right True
        , testCase "missing field" $ decodeJson (field "b" bool) "{\"a\":true}" @?= Left "missing field 'b'"
        , testCase "not an object" $ decodeJson (field "b" bool) "true" @?= Left "true is not an object"
        ]

decodeIndex :: TestTree
decodeIndex =
    testGroup
        "index"
        [ testCase "existing element" $ decodeJson (index 1 bool) "[null,true,1]" @?= Right True
        , testCase "non existing element" $ decodeJson (index 3 bool) "[null,true,1]" @?= Left "3 is not a valid index in array"
        , testCase "invalid type at index" $ decodeJson (index 1 int) "[null,true,1]" @?= Left "true is not an integer"
        , testCase "not an array" $ decodeJson (index 2 int) "true" @?= Left "true is not an array"
        ]
