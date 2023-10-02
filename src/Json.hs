module Json (parse, JsonValue (..)) where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char (chr, isHexDigit)
import Data.Functor (($>))
import Data.List (singleton, uncons)
import Data.Map (Map)
import Data.Map qualified as M (empty, fromList)
import Data.Tuple (swap)
import Prelude hiding (exponent)

-- |  PUBLIC API

-- | ADT representing a parsed JSON value
data JsonValue
    = JsBool Bool
    | JsNull
    | JsNum Double
    | JsStr String
    | JsArray [JsonValue]
    | JsObj (Map String JsonValue)
    deriving (Eq, Show)

-- |  Try to parse a string into a JsonValue.
parse :: String -> Maybe JsonValue
parse input = case runParser json input of
    Just ("", val) -> pure val -- succeed if there was no parse errors and all input was consumed
    _ -> fail "Parse error."

-- |  IMPLEMENTATION

{- | This is the main parser type used for this JSON parser.

A parser is a function that takes an input string and returns Just the parsed value and
the remaining input or Nothing if the parse fails.
-}
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- |  INSTANCES

-- | Lets make our parser a Functor so we can map it to other values.§
instance Functor Parser where
    fmap f pa = Parser $ fmap (fmap f) . runParser pa

{- | Why not go applicative while we're at it? This allows us to
combine parsers applicative style. Cool stuff!
-}
instance Applicative Parser where
    pure val = Parser $ \input -> pure (input, val)

    pab <*> pa = Parser $ \input -> do
        (input', f) <- runParser pab input
        (input'', a) <- runParser pa input'
        pure (input'', f a)

{- | Ohh, the dreaded Monad instance!!! Don't be afraid, it will
empower us to chain parsers that depend on each other.
-}
instance Monad Parser where
    return = pure
    pa >>= fab = Parser $ \input -> do
        (input', a) <- runParser pa input
        runParser (fab a) input'

{- |  Alternative is convenient when we want to try different parsers and
 choose the first one tha succeds.
-}
instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

-- |  SIMPLE PARSERS

-- |  Just get the next character from de input. Will fail if the input is empty.
next :: Parser Char
next = Parser $ fmap swap . uncons

-- | Parse a single specific character.
char :: Char -> Parser Char
char c = match (== c)

-- | Parse a single character that matches the predicate.
match :: (Char -> Bool) -> Parser Char
match predicate = do
    c <- next
    guard (predicate c)
    pure c

-- | Parse an exact string.
str :: String -> Parser String
str = mapM char

-- | PARSER COMBINATORS

-- |  Tries the parsers one by one and return the first one that succeeds.
oneOf :: [Parser a] -> Parser a
oneOf [] = empty
oneOf parsers = foldr1 (<|>) parsers

-- | Parse a sequence of monoid values and combine them into one result.
pconcat :: (Monoid a) => [Parser a] -> Parser a
pconcat parsers = mconcat <$> sequence parsers

{- | JSON PARSERS

In this section I have tried to define parsers that maps direcly to the grammar
https://www.json.org/json-en.html
-}

{- | Parse a JsonValue. This is the top level parser that can parse all
the different JSON values.
-}
json :: Parser JsonValue
json = element

value :: Parser JsonValue
value =
    oneOf
        [ jsObject
        , jsArray
        , jsString
        , jsNumber
        , jsTrue
        , jsFalse
        , jsNull
        ]

-- |  Parse a JSON object
jsObject :: Parser JsonValue
jsObject =
    JsObj
        <$> oneOf
            [ M.empty <$ (char '{' >> ws >> char '}')
            , M.fromList <$> (char '{' *> members <* char '}')
            ]

-- |  Parse zero or more key-value pairs separated by a comma (,).
members :: Parser [(String, JsonValue)]
members =
    oneOf
        [ (:) <$> member <*> (char ',' *> members) -- todo sepBy
        , singleton <$> member
        ]

{- |  Parse a single key-value pair where the key and the value is separated
  by a colon (:).
-}
member :: Parser (String, JsonValue)
member =
    (,) <$> (ws *> string <* ws <* char ':') <*> element

-- |  Parse a JSON array
jsArray :: Parser JsonValue
jsArray =
    JsArray
        <$> oneOf
            [ (char '[' >> ws >> char ']') $> []
            , char '[' *> elements <* char ']'
            ]

-- |  Parse one or more array elements separated by a comma (,).
elements :: Parser [JsonValue]
elements =
    oneOf
        [ (:) <$> element <*> (char ',' *> elements) -- todo sepBy
        , singleton <$> element
        ]

{- |  Parse an 'element', that is a JSON value that can be surrounded
 by whitespaces.
-}
element :: Parser JsonValue
element =
    ws *> value <* ws

-- | Parse a JSON string as a JsonValue
jsString :: Parser JsonValue
jsString = JsStr <$> string

-- | Parse a JSON string as a Haskell String.
string :: Parser String
string = char '"' *> characters <* char '"'

-- |  Parse zero or more characters inside a JSON string.
characters :: Parser String
characters =
    oneOf
        [ pconcat [character, characters]
        , pure ""
        ]

{- |  Parse one character inside a JSON string and
handle escaped characters correctly.
-}
character :: Parser String -- todo change type to Char
character =
    oneOf
        [ singleton <$> match (\c -> c /= '"' && c /= '\\')
        , match (== '\\') *> escape
        ]

-- | Parse one escaped character.
escape :: Parser String -- todo change type to char
escape =
    oneOf
        [ char '"' $> "\""
        , char '\\' $> "\\"
        , char '/' $> "/"
        , char 'b' $> "\b"
        , char 'f' $> "\f"
        , char 'n' $> "\n"
        , char 'r' $> "\r"
        , char 't' $> "\t"
        , char 'u' *> (singleton . hexStringToChar <$> pconcat [hex, hex, hex, hex])
        ]
  where
    hexStringToChar :: String -> Char
    hexStringToChar hexStr = chr $ read ("0x" ++ hexStr)

-- Parse one hex digit.
hex :: Parser String
hex = singleton <$> match isHexDigit

-- Parse a JSON number
jsNumber :: Parser JsonValue
jsNumber = JsNum . read <$> pconcat [integer, fraction, exponent]

-- Parse the integer part of a JSON number.
integer :: Parser String
integer =
    oneOf
        [ pconcat [str "-", onenine, digits]
        , pconcat [str "-", digit]
        , pconcat [onenine, digits]
        , pconcat [digit]
        ]

-- Parse the fraction part of a JSON number. The fraction can be bank.
fraction :: Parser String
fraction =
    oneOf
        [ pconcat [str ".", digits]
        , pure ""
        ]

-- Parse the exponent part of a JSON number. The exponent can be bank.
exponent :: Parser String
exponent =
    oneOf
        [ pconcat [str "E", sign, digits]
        , pconcat [str "e", sign, digits]
        , pure ""
        ]

-- Parse one digit 0 .. 9
digit :: Parser String -- todo change to char
digit = str "0" <|> onenine

-- Parse one or more digits
digits :: Parser String
digits = mconcat <$> some digit

-- Parse one digit 1..9
onenine :: Parser String -- todo change to char
onenine = singleton <$> match (\c -> '1' <= c && c <= '9')

-- Parse optional sign
sign :: Parser String
sign =
    oneOf
        [ str "+"
        , str "-"
        , pure ""
        ]

-- Parse zero or more whitespace characters
ws :: Parser String -- todo change to ()
ws = many (match $ flip elem ['\x0020', '\x000A', '\x000D', '\x0009'])

-- Parse JSON true
jsTrue :: Parser JsonValue
jsTrue = str "true" $> JsBool True

-- Parse JSON false
jsFalse :: Parser JsonValue
jsFalse = str "false" $> JsBool False

-- Parse JSON null
jsNull :: Parser JsonValue
jsNull = str "null" $> JsNull
