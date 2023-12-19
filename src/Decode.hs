{-# LANGUAGE LambdaCase #-}

module Decode (
    Decoder,
    double,
    decodeValue,
    decodeJson,
) where

import Data.Bool (bool)
import Data.List (intersperse)
import Data.Map.Strict (toList)
import Json (JsonValue (..))
import Json qualified

newtype Decoder a = Decoder {runDecoder :: JsonValue -> Either String a}

decodeJson :: Decoder a -> String -> Either String a
decodeJson decoder json = case Json.parse json of
    Just value -> decodeValue decoder value
    Nothing -> Left "invalid json"

decodeValue :: Decoder a -> JsonValue -> Either String a
decodeValue = runDecoder

double :: Decoder Double
double = Decoder $ \case
    JsNum n -> Right n
    other -> Left $ mconcat [toStr other, " is not a number"]

toStr :: JsonValue -> String
toStr = \case
    JsNum n -> show n
    JsNull -> "null"
    JsBool p -> bool "false" "true" p
    JsStr str -> mconcat ["\"", str, "\""]
    JsArray items -> mjoin "[" "," "]" $ fmap toStr items
    JsObj members -> mjoin "{" "," "}" $ fmap memberS (toList members)
  where
    memberS :: (String, JsonValue) -> String
    memberS (key, value) = mconcat ["\"", key, "\":", toStr value]

mjoin :: (Monoid a) => a -> a -> a -> [a] -> a
mjoin prefix separator postfix items = mconcat $ [prefix] ++ intersperse separator items ++ [postfix]