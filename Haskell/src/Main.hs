module Main (main) where

import System.Environment
import System.Exit
import System.Directory
import Data.List
import qualified Error
import qualified Lexer
import qualified Parser
import qualified Ast.Display as D
import qualified Analyzer

-- TODO: Maybe use a monad transformer with IO to handle early return

main :: IO ()
main = do
    args1 <- getArgs
    print args1
    let args = ["../a.sigma", "--print-tokens", "--print-ast"]
    
    case validateArgs args of
        [] -> return ()
        xs -> do
            mapM_ putStrLn xs
            exitWith $ ExitFailure 1
    
    let showHelp = "--help" `elem` args
        printAll = "--print-all" `elem` args
        printTokens = printAll || "--print-tokens" `elem` args
        printAst = printAll || "--print-ast" `elem` args
        enableOptimizations = "--fast" `elem` args
        onlyCompile = "--only-compile" `elem` args
        
    print (showHelp, printAll, printTokens, printAst, enableOptimizations, onlyCompile)

    if showHelp then do
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
                (sf : _) -> sf

    if null sourceFile
        then raiseError "Error: No source file provided"
        else return ()

    fileExists <- doesFileExist sourceFile
    if not fileExists 
        then raiseError $ "Error: File \"" ++ sourceFile ++ "\" doesn't exist"
        else return ()

    source <- readFile sourceFile
    tokens <- case Lexer.scan source of
        Left errors -> do
            mapM_ putStrLn (map Error.makeErrorMessage errors)
            exitWith $ ExitFailure 1
        Right tokens -> return tokens

    ast <- case Parser.parse tokens of
        Left e -> do
            putStrLn $ Error.makeErrorMessage e
            exitWith $ ExitFailure 1
        Right tree -> return tree
    
    let analyzerErrors = Analyzer.analyze ast
    if not . null $ analyzerErrors then do
        mapM_ putStrLn (map Error.makeErrorMessage analyzerErrors)
        exitWith $ ExitFailure 1
    else return ()
    
    if printTokens
        then mapM_ putStrLn (map show tokens)
        else return ()
    
    if printAst
        then putStrLn $ D.displayAst ast
        else return ()

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