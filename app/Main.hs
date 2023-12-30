module Main where

import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hPutStrLn, openFile, stderr, stdin)

import Json qualified
import System.Exit (exitFailure)
import Users qualified

main :: IO ()
main = do
    users <- Users.loadUsers "examples/users.json"
    mapM_ print users

main2 :: IO ()
main2 = do
    args <- getArgs
    case args of
        [] ->
            parseJsonFromHandle stdin
        [filename] -> do
            h <- openFile filename ReadMode
            parseJsonFromHandle h
            hClose h
        _ -> do
            hPutStrLn stderr "Wrong number of arguments."
            exitFailure

parseJsonFromHandle :: Handle -> IO ()
parseJsonFromHandle fileHandle = do
    result <- Json.parse <$> hGetContents fileHandle
    case result of
        Just json -> do
            putStrLn "Parsed JSON: "
            print json
        _ -> hPutStrLn stderr "Parse failure."
