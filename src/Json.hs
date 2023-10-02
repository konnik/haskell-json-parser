module Json (parse, JsonValue (..)) where

import Control.Applicative (Alternative (..))
import Control.Monad (guard, void)
import Data.Char (chr, isHexDigit)
import Data.Foldable1 (foldl1')
import Data.Functor (($>))
import Data.List (singleton, uncons)
import Data.Map (Map, foldl')
import Data.Map qualified as M (empty, fromList)
import Data.Tuple (swap)
import Prelude hiding (exponent)

data JsonValue
    = JsBool Bool
    | JsNull
    | JsNum Double
    | JsStr String
    | JsArray [JsonValue]
    | JsObj (Map String JsonValue)
    deriving (Eq, Show)

parse :: String -> Maybe JsonValue
parse input = case runParser json input of
    Just ("", val) -> pure val -- succeed if there was no parse errors and all input was consumed
    _ -> fail "Parse error."

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap f pa = Parser $ fmap (fmap f) . runParser pa

instance Applicative Parser where
    pure val = Parser $ \input -> pure (input, val)

    pab <*> pa = Parser $ \input -> do
        (input', f) <- runParser pab input
        (input'', a) <- runParser pa input'
        pure (input'', f a)

instance Monad Parser where
    return = pure
    pa >>= fab = Parser $ \input -> do
        (input', a) <- runParser pa input
        runParser (fab a) input'

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

next :: Parser Char
next = Parser $ fmap swap . uncons

ch :: Char -> Parser Char
ch c = match (== c)

match :: (Char -> Bool) -> Parser Char
match predicate = do
    c <- next
    guard (predicate c)
    pure c

oneOf :: [Parser a] -> Parser a
oneOf = foldr1 (<|>)

str :: String -> Parser String
str [] = pure []
str (x : xs) = do
    c <- match (== x)
    rest <- str xs
    pure $ c : rest

json :: Parser JsonValue
json = element

jsObject :: Parser JsonValue
jsObject = JsObj <$> object

object :: Parser (Map String JsonValue)
object =
    oneOf
        [ M.empty <$ (ch '{' >> ws >> ch '}')
        , M.fromList <$> (ch '{' *> members <* ch '}')
        ]

members :: Parser [(String, JsonValue)]
members =
    oneOf
        [ (:) <$> member <*> (ch ',' *> members) -- todo sepBy
        , singleton <$> member
        ]

member :: Parser (String, JsonValue)
member =
    (,) <$> (ws *> string <* ws <* ch ':') <*> element

jsArray :: Parser JsonValue
jsArray = JsArray <$> array

array :: Parser [JsonValue]
array =
    oneOf
        [ (ch '[' >> ws >> ch ']') $> []
        , ch '[' *> elements <* ch ']'
        ]

elements :: Parser [JsonValue]
elements =
    oneOf
        [ (:) <$> element <*> (ch ',' *> elements)
        , singleton <$> element
        ]

element :: Parser JsonValue
element =
    ws *> value <* ws

string :: Parser String
string = str "\"" *> characters <* str "\""

characters :: Parser String
characters =
    oneOf
        [ strOf [character, characters]
        , pure ""
        ]

character :: Parser String -- todo change type to Char
character =
    oneOf
        [ singleton <$> match (\c -> c /= '"' && c /= '\\')
        , match (== '\\') *> escape
        ]

escape :: Parser String
escape =
    oneOf
        [ ch '"' $> "\""
        , ch '\\' $> "\\"
        , ch '/' $> "/"
        , ch 'b' $> "\b"
        , ch 'f' $> "\f"
        , ch 'n' $> "\n"
        , ch 'r' $> "\r"
        , ch 't' $> "\t"
        , ch 'u' *> (singleton . hexStringToChar <$> strOf [hex, hex, hex, hex])
        ]

hex :: Parser String
hex = singleton <$> match isHexDigit

hexStringToChar :: String -> Char
hexStringToChar hexStr = chr $ read ("0x" ++ hexStr)

value :: Parser JsonValue
value =
    oneOf
        [ jsBool
        , jsNull
        , jsNumber
        , jsString
        , jsArray
        , jsObject
        ]

jsBool :: Parser JsonValue
jsBool = true <|> false
  where
    true = str "true" $> JsBool True
    false = str "false" $> JsBool False

jsNull :: Parser JsonValue
jsNull = str "null" $> JsNull

jsNumber :: Parser JsonValue
jsNumber = JsNum . read <$> number

jsString :: Parser JsonValue
jsString = JsStr <$> string

number :: Parser String
number = strOf [integer, fraction, exponent]

strOf :: [Parser String] -> Parser String
strOf parsers = concat <$> sequence parsers

integer :: Parser String
integer =
    oneOf
        [ strOf [str "-", onenine, digits]
        , strOf [str "-", digit]
        , strOf [onenine, digits]
        , strOf [digit]
        ]

fraction :: Parser String
fraction =
    oneOf
        [ strOf [str ".", digits]
        , pure ""
        ]

exponent :: Parser String
exponent =
    oneOf
        [ strOf [str "E", sign, digits]
        , strOf [str "e", sign, digits]
        , pure ""
        ]

digit :: Parser String
digit = str "0" <|> onenine

digits :: Parser String
digits = strOf [digit, digits] <|> digit

onenine :: Parser String
onenine = do
    c <- next
    guard $ '1' <= c && c <= '9'
    pure [c]

sign :: Parser String
sign =
    oneOf
        [ str "+"
        , str "-"
        , pure ""
        ]

ws :: Parser String
ws = many (match $ flip elem ['\x0020', '\x000A', '\x000D', '\x0009'])
