module Users (loadUsers, User) where

import Control.Monad (join)
import Decode (Decoder, field, int, list, nullable, optionalField, string)
import Decode qualified as D
import System.Exit (exitFailure)

loadUsers :: String -> IO [User]
loadUsers fileName = do
    content <- readFile fileName

    let result = D.decodeJson usersDecoder content

    case result of
        Right users -> pure users
        Left err -> do
            putStrLn ("Could not load users: " ++ err)
            exitFailure

data User
    = Guest String
    | Registered
        { id :: UserId
        , alias :: Alias
        , email :: Email
        , phone :: Maybe Phone
        }
    deriving (Eq, Show)

newtype UserId = UserId Int deriving (Eq, Show)
newtype Alias = Alias String deriving (Eq, Show)
newtype Email = Email String deriving (Eq, Show)
newtype Phone = Phone String deriving (Eq, Show)

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
    Registered
        <$> (UserId <$> field "id" int)
        <*> (Alias <$> field "alias" string)
        <*> (Email <$> field "email" string)
        <*> fmap (fmap Phone . join) (optionalField "phone" (nullable string))
