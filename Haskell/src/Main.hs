module Main (main) where

import System.Environment
import System.Exit
import System.Directory
import Data.List

main :: IO ()
main = do
    args1 <- getArgs
    print args1

    let args = ["../a.sigma"]
    validateArgs args
    
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
            [] -> do
                putStrLn "Error: No source file provided"
                exitWith (ExitFailure 1)
            (sourceName : _) -> do
                fileExists <- doesFileExist sourceName
                if fileExists then do
                    source <- readFile sourceName
                    putStrLn source
                else do
                    putStrLn $ "Error: File \"" ++ sourceName ++ "\" doesn't exist"
                    exitWith (ExitFailure 1)

validateArgs :: [String] -> IO ()
validateArgs args = case args of
    [] -> return ()
    ("--help" : xs)         -> validateArgs xs
    ("--print-ast" : xs)    -> validateArgs xs
    ("--print-tokens" : xs) -> validateArgs xs
    ("--print-all" : xs)    -> validateArgs xs
    ("--fast" : xs)         -> validateArgs xs
    ("--only-compile" : xs) -> validateArgs xs
    (x : xs)
        | ".sigma" `isSuffixOf` x -> validateArgs xs
        | otherwise -> do
            putStrLn $ "Error: Unknown option \"" ++ x ++ "\""
            exitWith (ExitFailure 1)