module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Map qualified as M

import Json (JsonValue (..), parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
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
        [ testCase "parse false" $ parse "false" @?= Just (JsBool False)
        , testCase "parse true" $ parse "true" @?= Just (JsBool True)
        ]

testNull :: TestTree
testNull =
    testGroup
        "Null"
        [ testCase "parse null" $ parse "null" @?= Just JsNull
        ]

testStrings :: TestTree
testStrings =
    testGroup
        "String"
        [ testParse "\"hello\"" "hello"
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
        [ testParse "[]" []
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
        [ testParse "{}" M.empty
        , testParse "{  \t}" M.empty
        , testParse "{\"one\": false}" $ M.fromList [("one", JsBool False)]
        , testParse "{\"one\": false, \"two\": 42}" $ M.fromList [("one", JsBool False), ("two", JsNum 42.0)]
        , testParse "{\"str\": \"a string\"}" $ M.fromList [("str", JsStr "a string")]
        , testParse "{\"arr\": [null, 42]}" $ M.fromList [("arr", JsArray [JsNull, JsNum 42.0])]
        , testParse "{\"obj\": {\"inner\":42}}" $ M.fromList [("obj", JsObj (M.fromList [("inner", JsNum 42.0)]))]
        ]
  where
    testParse input expectedMap = testCase ("parse '" ++ input ++ "'") $ parse input @?= Just (JsObj expectedMap)
