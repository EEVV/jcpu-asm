module Lexer where
import Control.Monad.State (State, state)
import Data.Char

import Error (Error (..))
import Token (TokenType (..), Token (..))


data Lexer = Lexer {
    source :: String,
    line :: Int
} deriving Show

-- get number with given first char (digit)
getNum :: Int -> Lexer -> Either Error (Lexer, Int)
getNum num (Lexer [] line) = Right (Lexer [] line, num)
getNum num (Lexer (x:xs) line)
    | isNumber x = getNum (num * 10 + digitToInt x) (Lexer xs line)
    | otherwise = Right (Lexer (x:xs) line, num)

-- get identifier with given first char
getIden :: Lexer -> String -> Either Error (Lexer, Token)
getIden (Lexer [] line) str = Right (Lexer [] line, Token (TokenIden str) line)
getIden (Lexer (x:xs) line) str
    | (isAlpha x) || (x == '_') = getIden (Lexer xs line) (str ++ [x])
    | otherwise = Right (Lexer (x:xs) line, Token (TokenIden str) line)

untilNewline :: Lexer -> Lexer
untilNewline lexer = case source lexer of
    [] -> lexer
    '\n':xs -> lexer
    x:xs -> untilNewline (lexer {source = xs})

getToken :: Lexer -> Either Error (Lexer, Token)
-- constant size tokens
-- todo cleanup
getToken lexer =
    let newState xs tokenType = Right (lexer {source = xs}, Token tokenType (Lexer.line lexer)) in
    case source lexer of
        '-':'>':xs -> newState xs TokenTo
        '#':xs -> getToken (untilNewline (lexer {source = xs}))
        '_':xs -> newState xs TokenEmpty
        '!':xs -> newState xs TokenNot
        '|':xs -> newState xs TokenOr
        '&':xs -> newState xs TokenAnd
        '^':xs -> newState xs TokenXor
        '-':xs -> newState xs TokenSub
        '+':xs -> newState xs TokenAdd
        '=':xs -> newState xs TokenEq
        '<':xs -> newState xs TokenLt
        '>':xs -> newState xs TokenGt
        '*':xs -> newState xs TokenMul
        '/':xs -> newState xs TokenDiv
        '?':xs -> newState xs TokenCond
        '(':xs -> newState xs TokenParenLeft
        ')':xs -> newState xs TokenParenRight
        '[':xs -> newState xs TokenSquareLeft
        ']':xs -> newState xs TokenSquareRight
        ',':xs -> newState xs TokenComma
        '\n':xs -> Right (lexer {source = xs, Lexer.line = 1 + Lexer.line lexer}, Token TokenNewline (Lexer.line lexer))
        '\t':xs -> newState xs TokenTab
        ' ':xs -> getToken (lexer {source = xs})
        'r':x:xs | isNumber x -> do
            (lexer, num) <- getNum (digitToInt x) (lexer {source = xs})
            return (lexer, Token (TokenReg num) (Lexer.line lexer))
        x:xs | isNumber x -> do
            (lexer, num) <- getNum (digitToInt x) (lexer {source = xs})
            return (lexer, Token (TokenNum num) (Lexer.line lexer))
        x:xs | isAlpha x -> do
            (lexer, token) <- getIden (lexer {source = xs}) [x]
            return (lexer, token)
        x:xs -> Left $ ErrorInvalidChar x (Lexer.line lexer)

getTokens :: Lexer -> Either Error [Token]
getTokens (Lexer [] line) = Right [Token TokenEof line]
getTokens lexer = do
    (lexer, token) <- getToken lexer
    tokens <- getTokens lexer
    return (token:tokens)

lex :: String -> Either Error [Token]
lex str = getTokens $ Lexer str 1

