module Users (loadUsers, User) where

import System.Exit (exitFailure)

import Decode (Decoder, decodeJson, field, int, list, optionalNullableField, string, validatedBy)
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
        , phone :: Phone
        }
    | RegisteredWithoutPhone
        { id :: UserId
        , alias :: Alias
        , email :: Email
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
registeredDecoder = do
    userId <- field "id" (int `validatedBy` parseId)
    alias <- Alias <$> field "alias" string
    email <- Email <$> field "email" string
    phone <- optionalNullableField "phone" (string `validatedBy` parsePhone)
    pure $ case phone of
        Nothing ->
            RegisteredWithoutPhone userId alias email
        Just p ->
            Registered userId alias email p

parseId :: Int -> Either String UserId
parseId n
    | n > 0 = Right (UserId n)
    | otherwise = Left "User id must be positive"

parsePhone :: String -> Either String Phone
parsePhone str =
    case str of
        '+' : _ -> Right $ Phone str
        _ -> Left "Phone number must start with a +"
