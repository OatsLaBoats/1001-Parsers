module Main (main) where

import System.Environment
import System.Exit
import System.Directory
import Data.List

import qualified Lexer

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
        exitWith ExitSuccess
    else return ()

    let sourceFile =
            case filter (isSuffixOf ".sigma") args of
                [] -> ""
                (sourceFile : _) -> sourceFile

    if null sourceFile
        then raiseError "Error: No source file provided"
        else return ()

    fileExists <- doesFileExist sourceFile
    if not fileExists 
        then raiseError $ "Error: File \"" ++ sourceFile ++ "\" doesn't exist"
        else return ()

    source <- readFile sourceFile
    putStrLn . show $ Lexer.scan source

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