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
    value,
    field,
    index,
    at,
    succeed,
    fail,
    oneOf,
) where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Bool qualified as Bool
import Data.List (foldl1', intersperse)
import Data.Map.Strict qualified as M (lookup, toList)
import Json (JsonValue (..))
import Json qualified
import Prelude hiding (fail)

newtype Decoder a = Decoder {runDecoder :: JsonValue -> Either String a}

instance Functor Decoder where
    fmap f fa = Decoder (fmap f . runDecoder fa)

instance Applicative Decoder where
    pure v = Decoder $ \_ -> Right v
    (<*>) :: Decoder (a -> b) -> Decoder a -> Decoder b
    fab <*> fa = fab >>= flip fmap fa

instance Monad Decoder where
    ma >>= f = Decoder $ \jsValue ->
        case runDecoder ma jsValue of
            Right a -> runDecoder (f a) jsValue
            Left errStr -> Left errStr

instance Alternative Decoder where
    empty :: Decoder a
    empty = Decoder $ \_ -> Left "Decoding failed."
    (<|>) :: Decoder a -> Decoder a -> Decoder a
    fa <|> fb = Decoder $ \jsValue ->
        case runDecoder fa jsValue of
            Left _ -> runDecoder fb jsValue
            Right x -> Right x

decodeJson :: Decoder a -> String -> Either String a
decodeJson decoder json = case Json.parse json of
    Just v -> decodeValue decoder v
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
            Just v -> decodeValue decoder v
            Nothing -> Left $ mconcat ["missing field '", fieldName, "'"]
    other -> Left $ mconcat [toStr other, " is not an object"]

index :: Int -> Decoder a -> Decoder a
index i decoder = Decoder $ \case
    JsArray elements ->
        if i >= 0 && i < length elements
            then runDecoder decoder (elements !! i)
            else Left $ show i ++ " is not a valid index in array"
    other ->
        Left $ mconcat [toStr other, " is not an array"]

value :: Decoder JsonValue
value = Decoder Right

at :: [String] -> Decoder a -> Decoder a
at [] decoder = decoder
at (x : xs) decoder = Decoder $ \jsValue -> do
    subValue <- runDecoder (field x value) jsValue
    runDecoder (at xs decoder) subValue

succeed :: a -> Decoder a
succeed = pure

fail :: String -> Decoder a
fail msg = Decoder $ const (Left msg)

oneOf :: [Decoder a] -> Decoder a
oneOf decoders = foldl1' (<|>) $ decoders ++ [fail "all oneOf decoders failed"]

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
    memberStr (key, val) = mconcat ["\"", key, "\":", toStr val]

mjoin :: (Monoid a) => a -> a -> a -> [a] -> a
mjoin prefix separator postfix items = mconcat $ [prefix] ++ intersperse separator items ++ [postfix]