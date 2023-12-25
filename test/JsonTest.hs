module JsonTest (test) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Map qualified as M

import Json (JsonValue (..), parse)

test :: TestTree
test =
    testGroup
        "JSON parser tests"
        [ testBooleans
        , testNull
        , testNumbers
        , testStrings
        , testArrays
        , testObjects
        ]

testNumbers :: TestTree
testNumbers =
    testGroup
        "Numbers"
        [ testParse "0" 0.0
        , testParse "42" 42.0
        , testParse "-42" (-42.0)
        , testParse "0.42" 0.42
        , testParse "-0" 0.0
        , testParse "3.1415" 3.1415
        , testParse "3.1415E-1" 0.31415
        , testParse "3.1415e-1" 0.31415
        , testParse "-42e+2" (-4200.0)
        , testParse "    -42e+2     " (-4200.0)
        ]
  where
    testParse input expected = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsNum expected)

testBooleans :: TestTree
testBooleans =
    testGroup
        "Booleans"
        [ testParse "  false  " False
        , testParse "true" True
        ]
  where
    testParse input expectedValue = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsBool expectedValue)

testNull :: TestTree
testNull =
    testGroup
        "Null"
        [ testParse "null" JsNull
        , testParse " null  " JsNull
        ]
  where
    testParse input expectedValue = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just expectedValue

testStrings :: TestTree
testStrings =
    testGroup
        "String"
        [ testParse "  \"hello\"  " "hello"
        , testParse "\"hello\\n\"" "hello\n"
        , testParse "\"hello\\t\"" "hello\t"
        , testParse "\"hello\\b\"" "hello\b"
        , testParse "\"hello\\r\"" "hello\r"
        , testParse "\"back\\\\slash\"" "back\\slash"
        , testParse "\"forward\\/slash\"" "forward/slash"
        , testParse "\"a \\\"quoted\\\" word\"" "a \"quoted\" word"
        , testParse "\"\"" ""
        , testParse "\" \"" " "
        , testParse "\"Unicode char: \\u00A9\"" "Unicode char: ©"
        ]
  where
    testParse input expectedStr = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsStr expectedStr)

testArrays :: TestTree
testArrays =
    testGroup
        "Arrays"
        [ testParse "  [] " []
        , testParse "[    ]" []
        , testParse "[   null ,  null,null]" [JsNull, JsNull, JsNull]
        , testParse
            "[\"hello\", false, true, null, 3.1415, [42]]"
            [JsStr "hello", JsBool False, JsBool True, JsNull, JsNum 3.1415, JsArray [JsNum 42]]
        ]
  where
    testParse input expectedArray = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsArray expectedArray)

testObjects :: TestTree
testObjects =
    testGroup
        "Objects"
        [ testParse "  {}  " []
        , testParse "{  \t}" []
        , testParse "{   \"one\": false   }" [("one", JsBool False)]
        , testParse "{\"one\": false, \"two\": 42}" [("one", JsBool False), ("two", JsNum 42.0)]
        , testParse "{\"one\": false, \"two\": 42, \"three\": null}" [("one", JsBool False), ("two", JsNum 42.0), ("three", JsNull)]
        , testParse "{\"str\": \"a string\"}" [("str", JsStr "a string")]
        , testParse "{\"arr\": [null, 42]}" [("arr", JsArray [JsNull, JsNum 42.0])]
        , testParse "{\"obj\": {\"inner\":42}}" [("obj", JsObj (M.fromList [("inner", JsNum 42.0)]))]
        , testParse "{\"duplicate\": false, \"duplicate\": true}" [("duplicate", JsBool True)]
        ]
  where
    testParse input expectedMap = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsObj $ M.fromList expectedMap)
