module Main where

import System.Environment (getArgs)
import System.IO (Handle, IOMode (..), hGetContents, hPutStrLn, openFile, stderr, stdin)

import Json qualified
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      parseJsonFromHandle stdin
    [filename] ->
      parseJsonFromHandle =<< openFile filename ReadMode
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
