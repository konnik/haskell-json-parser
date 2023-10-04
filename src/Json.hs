{- | This module contains a very simple implementation of a
recursive descent style JSON parser.
-}
module Json (parse, JsonValue (..)) where

import Control.Applicative (Alternative (..))
import Control.Monad (guard, void)
import Data.Char (chr, isHexDigit)
import Data.Functor (($>), (<&>))
import Data.List (singleton, uncons)
import Data.Map (Map)
import Data.Map qualified as M (fromList)
import Data.Tuple (swap)
import Prelude hiding (exponent, null)

-- * Public API

-- | ADT representing a parsed JSON value.
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

-- * Parser implementation

{- | This is the main parser type used for this JSON parser.

A parser is a function that takes an input string and returns Just the parsed value and
the remaining input or Nothing if the parse fails.
-}
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- *  Instances

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

{- | I think it will be very useful to be able to
combine the values of two parser so let's define Semigroup.
-}
instance (Semigroup a) => Semigroup (Parser a) where
    (<>) = liftA2 (<>)

-- | And why not go all the way to Monoid too...
instance (Monoid a) => Monoid (Parser a) where
    mempty = pure mempty

-- *  Basic parsers

-- | Definitions of some simple parsers that can be combined into more advanced ones.

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

-- | Parse an exact character sequence.
str :: String -> Parser String
str = mapM char

-- * Parser combinators

-- |  Defines combinators that can be used to build more avanced parsers.

{- | Try a list of parsers one by one and return the value of the
first parser that succeeds.
-}
oneOf :: [Parser a] -> Parser a
oneOf [] = empty -- fail if the list of parsers is empty
oneOf parsers = foldr1 (<|>) parsers

-- | Lifts cons operator (:) into the world of parsers.
(|:) :: Parser a -> Parser [a] -> Parser [a]
(|:) = liftA2 (:)

infixr 5 |:

-- | Parse a list of one or more values delimited by a specific character.
delimitedBy :: Parser a -> Char -> Parser [a]
delimitedBy elemP delimiter =
    oneOf
        [ elemP |: many (char delimiter *> elemP)
        , singleton <$> elemP
        ]

-- |  Parse a value that is surrounded by the specific characters.
surroundedBy :: Char -> Parser a -> Char -> Parser a
surroundedBy left parser right = char left *> parser <* char right

-- * JSON Parsers

{- | In this section I have tried to define parsers that maps direcly to the grammar
found on https://www.json.org/json-en.html
-}

{- | Parse a JSON value. This is the top level parser that can parse all
the different JSON values.
-}
json :: Parser JsonValue
json = element

{- | Parse a JSON value. This parser does not trim whitespaces so it will fail if the
value is surrounded by whitespace.
-}
value :: Parser JsonValue
value =
    oneOf
        [ object <&> JsObj
        , array <&> JsArray
        , string <&> JsStr
        , number <&> JsNum
        , true <&> JsBool
        , false <&> JsBool
        , null $> JsNull
        ]

-- |  Parse a JSON object
object :: Parser (Map String JsonValue)
object =
    M.fromList
        <$> oneOf
            [ surroundedBy '{' ws '}' $> []
            , surroundedBy '{' members '}'
            ]

-- |  Parse one or more key-value pairs separated by a comma (,).
members :: Parser [(String, JsonValue)]
members = member `delimitedBy` ','

{- |  Parse a single key-value pair where the key and the value is separated
  by a colon (:).
-}
member :: Parser (String, JsonValue)
member = (,) <$> key <*> element
  where
    key = ws *> string <* ws <* char ':'

-- |  Parse a JSON array
array :: Parser [JsonValue]
array =
    oneOf
        [ surroundedBy '[' ws ']' $> []
        , surroundedBy '[' elements ']'
        ]

-- |  Parse one or more array elements separated by a comma (,).
elements :: Parser [JsonValue]
elements = element `delimitedBy` ','

{- |  Parse an 'element', that is a JSON value that can be surrounded
 by whitespaces.
-}
element :: Parser JsonValue
element = ws *> value <* ws

-- | Parse a JSON string
string :: Parser String
string = surroundedBy '"' characters '"'

-- |  Parse zero or more characters inside a JSON string.
characters :: Parser String
characters = many character

{- |  Parse one character inside a JSON string and
handle escaped characters correctly.
-}
character :: Parser Char
character =
    oneOf
        [ match (`notElem` ['"', '\\'])
        , char '\\' >> escape
        ]

-- | Parse one escaped character.
escape :: Parser Char
escape =
    oneOf
        [ char '"' $> '"'
        , char '\\' $> '\\'
        , char '/' $> '/'
        , char 'b' $> '\b'
        , char 'f' $> '\f'
        , char 'n' $> '\n'
        , char 'r' $> '\r'
        , char 't' $> '\t'
        , char 'u' >> sequence [hex, hex, hex, hex] <&> hexToChar
        ]
  where
    hexToChar :: String -> Char
    hexToChar hexStr = chr $ read ('0' : 'x' : hexStr)

-- |  Parse one hex digit.
hex :: Parser Char
hex = match isHexDigit

-- |  Parse a JSON number
number :: Parser Double
number = read <$> mconcat [integer, fraction, exponent]

-- |  Parse the integer part of a JSON number.
integer :: Parser String
integer =
    oneOf
        [ char '-' |: onenine |: digits
        , char '-' |: digit |: pure []
        , onenine |: digits
        , digit |: pure []
        ]

-- |  Parse the fraction part of a JSON number. The fraction can be bank.
fraction :: Parser String
fraction =
    oneOf
        [ char '.' |: digits
        , mempty
        ]

{- |  Parse the optional exponent part of a JSON number.
If no exponent can be parsed the empty string is returned.
-}
exponent :: Parser String
exponent =
    oneOf
        [ mconcat [str "E", sign, digits]
        , mconcat [str "e", sign, digits]
        , mempty
        ]

-- |  Parse one digit 0..9
digit :: Parser Char
digit = char '0' <|> onenine

-- |  Parse one digit 1..9
onenine :: Parser Char
onenine = match (`elem` ['1' .. '9'])

-- |  Parse one or more digits
digits :: Parser String
digits = some digit

-- |  Parse optional sign
sign :: Parser String
sign =
    oneOf
        [ str "+"
        , str "-"
        , pure ""
        ]

-- | Parse zero or more whitespace characters
ws :: Parser ()
ws = void $ many $ match (`elem` ['\x0020', '\x000A', '\x000D', '\x0009'])

-- | Parse JSON true
true :: Parser Bool
true = str "true" $> True

-- | Parse JSON false
false :: Parser Bool
false = str "false" $> False

-- | Parse JSON null
null :: Parser ()
null = str "null" $> ()
