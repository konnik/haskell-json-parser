module Users (loadUsers, User) where

import Control.Monad (join)
import System.Exit (exitFailure)

import Decode (Decoder, decodeJson, field, int, list, nullable, optionalField, string, validatedBy)
import Decode qualified as D

loadUsers :: String -> IO [User]
loadUsers fileName = do
    content <- readFile fileName

    let result = decodeJson usersDecoder content

    case result of
        Right users -> pure users
        Left err -> do
            putStrLn ("Could not load users: " ++ err)
            exitFailure

data User
    = Guest
        { displayName :: String
        }
    | Registered
        { id :: UserId
        , alias :: Alias
        , email :: Email
        , phone :: Maybe Phone
        }
    deriving (Show)

newtype UserId = UserId Int deriving (Show)
newtype Alias = Alias String deriving (Show)
newtype Email = Email String deriving (Show)
newtype Phone = Phone String deriving (Show)

usersDecoder :: Decoder [User]
usersDecoder = field "users" (list userDecoder)

userDecoder :: Decoder User
userDecoder = do
    typ <- field "type" string
    case typ of
        "guest" -> guestDecoder
        "registered" -> registeredDecoder
        other -> D.fail $ "Illegal type: " ++ other

guestDecoder :: Decoder User
guestDecoder = Guest <$> field "displayName" string

registeredDecoder :: Decoder User
registeredDecoder =
    Registered <$> userId <*> alias <*> email <*> phone
  where
    userId = UserId <$> field "id" int
    alias = Alias <$> field "alias" string
    email = Email <$> field "email" string
    phone :: Decoder (Maybe Phone)
    phone =
        join
            <$> optionalField
                "phone"
                (nullable (string `validatedBy` parsePhone))

parsePhone :: String -> Either String Phone
parsePhone str =
    case str of
        '+' : _ -> Right $ Phone str
        _ -> Left "Phone number must start with a +"
