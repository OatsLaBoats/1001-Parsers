module Main (main) where

import System.Environment
import System.Exit
import System.Directory
import Data.List

import Lexer

main :: IO ()
main = do
    args1 <- getArgs
    print args1
    let args = ["../a.sigma"]
    
    case validateArgs args of
        [] -> return ()
        xs -> do
            mapM_ putStrLn xs
            exitWith (ExitFailure 1)
    
    if "--help" `elem` args then do
        putStrLn "Usage: sigma [options...] \"source file\"\n\
                 \Options:\n\
                 \         --help          Displays this message.\n\
                 \         --print-ast     Prints out the synatax tree.\n\
                 \         --print-tokens  Prints out the stream of lexer tokens.\n\
                 \         --print-all     Enables all printing functionality.\n\
                 \         --fast          Enables optimization.\n\
                 \         --only-compile  Compiles the script without running it."
    else
        case filter (isSuffixOf ".sigma") args of
            [] -> raiseError "Error: No source file provided"
            (sourceName : _) -> do
                fileExists <- doesFileExist sourceName
                if not fileExists 
                then raiseError $ "Error: File \"" ++ sourceName ++ "\" doesn't exist"
                else do
                    source <- readFile sourceName
                    putStrLn . show $ scan source

raiseError :: String -> IO ()                   
raiseError s = do
    putStrLn s
    exitWith (ExitFailure 1)

validateArgs :: [String] -> [String]
validateArgs args = case args of
    [] -> []
    ("--help" : xs)         -> validateArgs xs
    ("--print-ast" : xs)    -> validateArgs xs
    ("--print-tokens" : xs) -> validateArgs xs
    ("--print-all" : xs)    -> validateArgs xs
    ("--fast" : xs)         -> validateArgs xs
    ("--only-compile" : xs) -> validateArgs xs
    (x : xs)
        | ".sigma" `isSuffixOf` x -> validateArgs xs
        | otherwise -> ("Error: Unknown option \"" ++ x ++ "\"") : validateArgs xs