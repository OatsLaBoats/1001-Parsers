module Lexer 
    ( Token(..)
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

data Token 
    = TkFun Location
    | TkReturn Location
    | TkPrint Location
    | TkVar Location
    | TkIf Location
    | TkElse Location
    | TkElif Location
    | TkWhile Location
    | TkIdentifier Lexeme Location
    | TkNumberLit Lexeme Location
    | TkStringLit Lexeme Location
    | TkBoolLit Lexeme Location
    | TkPlus Location
    | TkMinus Location
    | TkMul Location
    | TkDiv Location
    | TkMod Location
    | TkEq Location
    | TkNeq Location
    | TkGt Location
    | TkLt Location
    | TkGtEq Location
    | TkLtEq Location
    | TkAnd Location
    | TkOr Location
    | TkNot Location
    | TkLParen Location   -- (
    | TkRParen Location   -- )
    | TkLBrace Location   -- {
    | TkRBrace Location   -- }
    | TkLBracket Location -- [
    | TkRBracket Location -- ]
    | TkEqual Location
    | TkColon Location
    | TkComma Location
    | TkLineEnd Location
    | TkEOF
    deriving Show

-- TODO: Change the name of bindings in functions some of them are terrible

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

    (' ' : rest)  -> noToken rest
    ('\t' : rest) -> noToken rest
    ('\r' : rest) -> noToken rest

    ('#' : rest)  -> skipLine s rest
    
    ('\n' : rest) -> 
        scanTokens s { getTokens = (TkLineEnd location) : tokens, getLocation = (line + 1, 1)} rest

    ('+' : rest) -> simpleToken (TkPlus location) rest
    ('-' : rest) -> simpleToken (TkMinus location) rest
    ('*' : rest) -> simpleToken (TkMul location) rest
    ('/' : rest) -> simpleToken (TkDiv location) rest
    ('%' : rest) -> simpleToken (TkMod location) rest
    
    ('(' : rest) -> simpleToken (TkLParen location) rest
    (')' : rest) -> simpleToken (TkRParen location) rest
    ('{' : rest) -> simpleToken (TkLBrace location) rest
    ('}' : rest) -> simpleToken (TkRBrace location) rest
    ('[' : rest) -> simpleToken (TkLBracket location) rest
    (']' : rest) -> simpleToken (TkRBracket location) rest
    (':' : rest) -> simpleToken (TkColon location) rest
    (',' : rest) -> simpleToken (TkComma location) rest
    
    ('=' : '=' : rest) -> simpleToken2 (TkEq location) rest
    ('=' : rest)       -> simpleToken (TkEqual location) rest
    
    ('!' : '=' : rest) -> simpleToken2 (TkNeq location) rest
    ('!' : rest)       -> makeError "Incomplete '!=' operator" rest
    
    ('>' : '=' : rest) -> simpleToken2 (TkGtEq location) rest
    ('>' : rest)       -> simpleToken (TkGt location) rest

    ('<' : '=' : rest) -> simpleToken2 (TkLtEq location) rest
    ('<' : rest)       -> simpleToken (TkLt location) rest
    
    ('"' : rest) -> scanString s { getLocation = (line, column + 1) } rest
    
    (c : rest)
        | isDigit c               -> scanNumber s src
        | isIdentifierStartChar c -> scanIdentifier s src
        | otherwise               -> makeError "Unexpected character" rest
    
    where
        tokens = getTokens s
        errors = getErrors s
        location = getLocation s
        (line, column) = location

        noToken = 
            scanTokens s { getLocation = (line, column + 1) }
        
        simpleToken token =
            scanTokens s { getTokens = token : tokens, getLocation = (line, column + 1) }
        
        simpleToken2 token =
            scanTokens s { getTokens = token : tokens, getLocation = (line, column + 2) }
        
        makeError message = 
            scanTokens s { getErrors = (message, location) : errors, getLocation = (line, column + 1) }

skipLine :: LexerState -> Source -> LexerState
skipLine s src = case src of
    []            -> scanTokens s src
    ('\n' : _) -> scanTokens s src
    (_ : rest)    -> skipLine s { getLocation = (line, column + 1) } rest
    where
        (line, column) = getLocation s

scanIdentifier :: LexerState -> Source -> LexerState
scanIdentifier s src = scanIdentifier_ s src "" (getLocation s)

scanIdentifier_ :: LexerState -> Source -> String -> Location -> LexerState
scanIdentifier_ s src ident startLoc = case src of
    [] -> makeToken
    (c : rest)
        | isIdentifierChar c -> scanIdentifier_ s { getLocation = (line, column + 1) } rest (c : ident) startLoc
        | otherwise -> makeToken
    where
        tokens = getTokens s
        location = getLocation s
        (line, column) = location
        makeToken = scanTokens s { getTokens = makeIdentifierToken : tokens } src

        correctIdent = reverse ident
        makeIdentifierToken = case correctIdent of
            "fun"    -> TkFun startLoc
            "return" -> TkReturn startLoc
            "print"  -> TkPrint startLoc
            "var"    -> TkVar startLoc
            "if"     -> TkIf startLoc
            "else"   -> TkElse startLoc
            "elif"   -> TkElif startLoc
            "while"  -> TkWhile startLoc
            "and"    -> TkAnd startLoc
            "or"     -> TkOr startLoc
            "not"    -> TkNot startLoc
            "true"   -> TkBoolLit "true" startLoc
            "false"  -> TkBoolLit "false" startLoc
            _        -> TkIdentifier correctIdent startLoc
 
isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = c `elem` (concat [['A'..'Z'], ['a'..'z'], ['_']])       

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierStartChar c || isDigit c

scanNumber :: LexerState -> Source -> LexerState
scanNumber s src = scanNumber_ s src "" (getLocation s)

scanNumber_ :: LexerState -> Source -> String -> Location -> LexerState
scanNumber_ s src num startLoc = case src of
    [] -> makeNumber
    ['.'] -> floatError ""
    ('.' : c : rest)
        | isDigit c -> scanNumber_ s { getLocation = (line, column + 1) } (c : rest) ('.' : num) startLoc
        | otherwise -> floatError (c : rest)
    (c : rest)
        | isDigit c -> scanNumber_ s { getLocation = (line, column + 1) } rest (c : num) startLoc
        | isIdentifierStartChar c -> 
            scanTokens s { getErrors = ("Identifiers can't begin with numbers", location) : errors } src
        | otherwise -> makeNumber
    where
        tokens = getTokens s
        errors = getErrors s
        location = getLocation s
        (line, column) = location
        
        makeNumber = 
            scanTokens s { getTokens = TkNumberLit (reverse num) startLoc : tokens, getLocation = (line, column + 1) } src
        
        floatError =
            scanTokens s { getErrors = ("Invalid 'Float' syntax", location) : errors }

scanString :: LexerState -> Source -> LexerState
scanString s src = scanString_ s src "" (getLocation s)

scanString_ :: LexerState -> Source -> String -> Location -> LexerState
scanString_ s src str startLoc = case src of
    []           -> errorState
    ('\n' : _)   -> errorState
    ('"' : rest) -> scanTokens s { getTokens = (TkStringLit (reverse str) startLoc) : tokens , getLocation = (line, column + 1) } rest
    (c : rest)   -> scanString_ s { getLocation = (line, column + 1) } rest (c : str) startLoc
    where
        tokens = getTokens s
        errors = getErrors s
        location = getLocation s
        (line, column) = location
        
        errorState = 
            scanTokens s { getErrors = ("String literal is not closed", startLoc) : errors } src