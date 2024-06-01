module Main (main) where

import System.Environment
import System.Exit
import System.Directory
import Data.List
import Control.Monad
import qualified Error
import qualified Lexer
import qualified Parser
import qualified Ast.Display as D
import qualified Analyzer
import qualified Interpreter

-- TODO: Maybe use a monad transformer with IO to handle early return

main :: IO ()
main = do
    args1 <- getArgs
    print args1
    let args = ["../test.sigma"]

    case validateArgs args of
        [] -> return ()
        xs -> do
            mapM_ putStrLn xs
            exitFailure
    
    let showHelp = "--help" `elem` args
        printAll = "--print-all" `elem` args
        printTokens = printAll || "--print-tokens" `elem` args
        printAst = printAll || "--print-ast" `elem` args
        enableOptimizations = "--fast" `elem` args
        onlyCompile = "--only-compile" `elem` args
        
    print (showHelp, printAll, printTokens, printAst, enableOptimizations, onlyCompile)

    when showHelp $ do
        putStrLn "Usage: sigma [options...] \"source file\"\n\
                 \Options:\n\
                 \         --help          Displays this message.\n\
                 \         --print-ast     Prints out the synatax tree.\n\
                 \         --print-tokens  Prints out the stream of lexer tokens.\n\
                 \         --print-all     Enables all printing functionality.\n\
                 \         --fast          Enables optimization.\n\
                 \         --only-compile  Compiles the script without running it."
        exitSuccess

    let sourceFile =
            case filter (isSuffixOf ".sigma") args of
                [] -> ""
                (sf : _) -> sf

    when (null sourceFile) $
        raiseError "Error: No source file provided"

    fileExists <- doesFileExist sourceFile
    unless fileExists $
        raiseError $ "Error: File \"" ++ sourceFile ++ "\" doesn't exist"

    source <- readFile sourceFile
    tokens <- case Lexer.scan source of
        Right tokens -> return tokens
        Left errors -> do
            mapM_ (putStrLn . Error.makeErrorMessage) errors
            exitFailure

    ast <- case Parser.parse tokens of
        Right tree -> return tree
        Left e -> do
            putStrLn $ Error.makeErrorMessage e
            exitFailure
    
    let analyzerErrors = Analyzer.analyze ast
    unless (null analyzerErrors) $ do
        mapM_ (putStrLn . Error.makeErrorMessage) analyzerErrors
        exitFailure
    
    when printTokens $
        mapM_ print tokens
    
    when printAst $
        putStrLn $ D.displayAst ast

    unless onlyCompile $ do
        exitCode <- Interpreter.eval ast
        print exitCode
        when (exitCode /= 0) $ exitWith $ ExitFailure exitCode

raiseError :: String -> IO ()                   
raiseError s = do
    putStrLn s
    exitWith $ ExitFailure 1

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
