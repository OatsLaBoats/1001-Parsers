module Lexer 
    ( Token(..)
    , TokenKind(..)
    , scan
    ) where

import Data.Char
import Error

type Source = String
type Lexeme = String

data LexerState = LexerState
    { getTokens :: [Token]
    , getErrors :: [Error]
    , getLocation :: Location
    }

data TokenKind
    = TkFun
    | TkReturn
    | TkPrint
    | TkVar
    | TkIf
    | TkElif
    | TkElse
    | TkWhile
    | TkIdentifier
    | TkNumberLit
    | TkStringLit
    | TkBoolLit
    | TkPlus
    | TkMinus
    | TkMul
    | TkDiv
    | TkMod
    | TkEq
    | TkNeq
    | TkGt
    | TkLt
    | TkGtEq
    | TkLtEq
    | TkAnd
    | TkOr
    | TkNot
    | TkLParen
    | TkRParen
    | TkLBrace
    | TkRBrace
    | TkLBracket
    | TkRBracket
    | TkEqual
    | TkColon
    | TkComma
    | TkLineEnd
    | TkEOF
    deriving (Show, Eq)

data Token = Token
    { getTokenKind :: TokenKind
    , getTokenLexeme :: String
    , getTokenLocation :: Location
    }
    deriving Show

scan :: Source -> Either [Error] [Token]
scan src = 
    case getErrors lexResult of
        []     -> Right . reverse . getTokens $ lexResult
        errors -> Left . reverse $ errors
    where
        lexResult = scanTokens (LexerState [] [] (1, 1)) src

scanTokens :: LexerState -> Source -> LexerState
scanTokens s src = case src of
    []            -> s

    (' ' : rest)  -> skip rest
    ('\t' : rest) -> skip rest
    ('\r' : rest) -> skip rest

    ('#' : rest)  -> skipLine s rest
    
    ('\n' : rest) -> advanceN (line + 1, 1) (makeToken TkLineEnd "\\n") rest

    ('+' : rest) -> advance (makeToken TkPlus "+") rest
    ('-' : rest) -> advance (makeToken TkMinus "-") rest
    ('*' : rest) -> advance (makeToken TkMul "*") rest
    ('/' : rest) -> advance (makeToken TkDiv "/") rest
    ('%' : rest) -> advance (makeToken TkMod "%") rest
    
    ('(' : rest) -> advance (makeToken TkLParen "(") rest
    (')' : rest) -> advance (makeToken TkRParen ")") rest
    ('{' : rest) -> advance (makeToken TkLBrace "{") rest
    ('}' : rest) -> advance (makeToken TkRBrace "}") rest
    ('[' : rest) -> advance (makeToken TkLBracket "[") rest
    (']' : rest) -> advance (makeToken TkRBracket "]") rest
    (':' : rest) -> advance (makeToken TkColon ":") rest
    (',' : rest) -> advance (makeToken TkComma ",") rest
    
    ('=' : '=' : rest) -> advance2 (makeToken TkEq "==") rest
    ('=' : rest)       -> advance (makeToken TkEqual "=") rest
    
    ('!' : '=' : rest) -> advance2 (makeToken TkNeq "!=") rest
    ('!' : rest)       -> advanceError "Incomplete '!=' operator" rest
    
    ('>' : '=' : rest) -> advance2 (makeToken TkGtEq ">=") rest
    ('>' : rest)       -> advance (makeToken TkGt ">") rest

    ('<' : '=' : rest) -> advance2 (makeToken TkLtEq "<=") rest
    ('<' : rest)       -> advance (makeToken TkLt "<") rest
    
    ('"' : rest) -> scanString s { getLocation = (line, column + 1) } rest
    
    (c : rest)
        | isDigit c               -> scanNumber s src
        | isIdentifierStartChar c -> scanIdentifier s src
        | otherwise               -> advanceError "Unexpected character" rest
    
    where
        tokens = getTokens s
        errors = getErrors s
        location = getLocation s
        (line, column) = location

        makeToken kind lexeme = Token kind lexeme location

        skip = scanTokens s { getLocation = (line, column + 1) }
        advanceN loc token = scanTokens s { getTokens = token : tokens, getLocation = loc }
        advance = advanceN (line, column + 1)
        advance2 = advanceN (line, column + 2)
        advanceError message = scanTokens s { getErrors = (message, location) : errors, getLocation = (line, column + 1) }

skipLine :: LexerState -> Source -> LexerState
skipLine s src = case src of
    []            -> scanTokens s src
    ('\n' : _) -> scanTokens s src
    (_ : rest)    -> skipLine s { getLocation = (line, column + 1) } rest
    where
        (line, column) = getLocation s

scanIdentifier :: LexerState -> Source -> LexerState
scanIdentifier s src = scanIdentifier_ (getLocation s) s src "" 

scanIdentifier_ :: Location -> LexerState -> Source -> String -> LexerState
scanIdentifier_ startLocation s src ident = case src of
    [] -> advance
    (c : rest)
        | isIdentifierChar c -> next rest (c : ident)
        | otherwise -> advance
    where
        tokens = getTokens s
        location = getLocation s
        (line, column) = location
        
        next = scanIdentifier_ startLocation s { getLocation = (line, column + 2) }
        advance = scanTokens s { getTokens = makeToken : tokens } src
        makeToken = case identifier of
            "fun"    -> Token TkFun identifier startLocation
            "return" -> Token TkReturn identifier startLocation
            "print"  -> Token TkPrint identifier startLocation
            "var"    -> Token TkVar identifier startLocation
            "if"     -> Token TkIf identifier startLocation
            "else"   -> Token TkElse identifier startLocation
            "elif"   -> Token TkElif identifier startLocation
            "while"  -> Token TkWhile identifier startLocation
            "and"    -> Token TkAnd identifier startLocation
            "or"     -> Token TkOr identifier startLocation
            "not"    -> Token TkNot identifier startLocation
            "true"   -> Token TkBoolLit identifier startLocation
            "false"  -> Token TkBoolLit identifier startLocation
            _        -> Token TkIdentifier identifier startLocation
            where identifier = reverse ident
 
isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = c `elem` (concat [['A'..'Z'], ['a'..'z'], ['_']])       

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierStartChar c || isDigit c

scanNumber :: LexerState -> Source -> LexerState
scanNumber s src = scanNumber_ (getLocation s) s src ""

scanNumber_ :: Location -> LexerState -> Source -> String -> LexerState
scanNumber_ startLocation s src num = case src of
    [] -> advance
    ['.'] -> advanceError floatError ""
    ('.' : c : rest)
        | isDigit c -> next (c : rest) ('.' : num)
        | otherwise -> advanceError floatError (c : rest)
    (c : rest)
        | isDigit c -> next rest (c : num)
        | isIdentifierStartChar c -> advanceError "Identifiers can't begin with numbers" src
        | otherwise -> advance
    where
        tokens = getTokens s
        errors = getErrors s
        location = getLocation s
        (line, column) = location
        
        makeToken = Token TkNumberLit (reverse num) startLocation
        next = scanNumber_ startLocation s { getLocation = (line, column + 1) }
        advance = scanTokens s { getTokens = makeToken : tokens, getLocation = (line, column + 1) } src
        advanceError message = scanTokens s { getErrors = (message, location) : errors }
        floatError = "Invalid 'Float' syntax"

scanString :: LexerState -> Source -> LexerState
scanString s src = scanString_ (getLocation s) s src ""

scanString_ :: Location -> LexerState -> Source -> String -> LexerState
scanString_ startLocation s src str = case src of
    []           -> advanceError
    ('\n' : _)   -> advanceError
    ('"' : rest) -> advance rest
    (c : rest)   -> next rest (c : str)
    where
        tokens = getTokens s
        errors = getErrors s
        (line, column) = getLocation s
        
        makeToken = Token TkStringLit (reverse str) startLocation
        next = scanString_ startLocation s { getLocation = (line, column + 1) }
        advance = scanTokens s { getTokens = makeToken : tokens, getLocation = (line, column + 1) }
        advanceError = scanTokens s { getErrors = ("String literal isn't closed", startLocation) : errors } src