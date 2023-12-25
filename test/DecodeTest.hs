module DecodeTest (test) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Decode
import Json (JsonValue (..))

test :: TestTree
test =
    testGroup
        "Decoder tests"
        [korv]

korv :: TestTree
korv =
    testGroup
        "double"
        [ testCase "3.14" $ decodeJson double "3.14" @?= Right 3.14
        , testCase "314" $ decodeJson double "314" @?= Right 314
        , testCase "-42.0" $ decodeJson double "-42.0" @?= Right (-42.0)
        , testCase "true" $ decodeJson double "true" @?= Left "true is not a double"
        ]
