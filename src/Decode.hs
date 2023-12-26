{-# LANGUAGE LambdaCase #-}

module Decode (
    Decoder,
    double,
    int,
    bool,
    decodeValue,
    decodeJson,
    string,
    list,
    field,
) where

import Data.Bool qualified as Bool
import Data.List (intersperse)
import Data.Map.Strict qualified as M (lookup, member, toList)
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
    other -> Left $ mconcat [toStr other, " is not a double"]

int :: Decoder Int
int = Decoder $ \case
    JsNum n ->
        if isInteger n
            then Right $ round n
            else Left $ mconcat [show n, " is not an integer"]
    other -> Left $ mconcat [toStr other, " is not an integer"]
  where
    isInteger :: Double -> Bool
    isInteger y = y == fromInteger (round y)

bool :: Decoder Bool
bool = Decoder $ \case
    JsBool b -> Right b
    other -> Left $ mconcat [toStr other, " is not a boolean"]

string :: Decoder String
string = Decoder $ \case
    JsStr str -> Right str
    other -> Left $ mconcat [toStr other, " is not a string"]

list :: Decoder a -> Decoder [a]
list itemDecoder = Decoder $ \case
    JsArray arr -> mapM (decodeValue itemDecoder) arr
    other -> Left $ mconcat [toStr other, " is not an array"]

field :: String -> Decoder a -> Decoder a
field fieldName decoder = Decoder $ \case
    JsObj members ->
        case M.lookup fieldName members of
            Just value -> decodeValue decoder value
            Nothing -> Left $ mconcat ["missing field '", fieldName, "'"]
    other -> Left $ mconcat [toStr other, " is not an object"]

toStr :: JsonValue -> String
toStr = \case
    JsNum n -> show n
    JsNull -> "null"
    JsBool p -> Bool.bool "false" "true" p
    JsStr str -> mconcat ["\"", str, "\""]
    JsArray items -> mjoin "[" "," "]" $ fmap toStr items
    JsObj members -> mjoin "{" "," "}" $ fmap memberStr (M.toList members)
  where
    memberStr :: (String, JsonValue) -> String
    memberStr (key, value) = mconcat ["\"", key, "\":", toStr value]

mjoin :: (Monoid a) => a -> a -> a -> [a] -> a
mjoin prefix separator postfix items = mconcat $ [prefix] ++ intersperse separator items ++ [postfix]