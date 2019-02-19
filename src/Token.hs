module Token where

data TokenType = TokenEmpty
    | TokenNum Int
    | TokenReg Int
    | TokenIden String
    | TokenTab
    | TokenNewline
    -- start operations
    | TokenTo 
    | TokenNot
    | TokenOr
    | TokenAnd
    | TokenXor
    | TokenAdd
    | TokenSub
    | TokenLeftShift
    | TokenRightShift
    | TokenSignedLeftShift
    | TokenSignedRightShift
    -- todo fix eq
    | TokenEq
    | TokenLt
    | TokenSignedLt
    | TokenMul
    | TokenDiv
    -- end operations
    | TokenCond
    | TokenParenLeft
    | TokenParenRight
    | TokenSquareLeft
    | TokenSquareRight
    | TokenComma
    | TokenEof

instance Show TokenType where
    show TokenEmpty = show "_"
    show (TokenNum n) = show n
    show (TokenReg r) = show $ "r" ++ (show r)
    show (TokenIden s) = show s
    show TokenTab = "tab"
    show TokenNewline = "newline"
    show TokenTo = show "->"
    show TokenNot = show "!"
    show TokenOr = show "|"
    show TokenAnd = show "&"
    show TokenXor = show "^"
    show TokenAdd = show "+"
    show TokenSub = show "-"
    show TokenEq = show "="
    show TokenLt = show "<"
    show TokenSignedLt = show "$<"
    show TokenMul = show "*"
    show TokenDiv = show "/"
    show TokenLeftShift = show "<<"
    show TokenRightShift = show ">>"
    show TokenSignedLeftShift = show "<<<"
    show TokenSignedRightShift = show ">>>"
    show TokenCond = show "?"
    show TokenParenLeft = show "("
    show TokenParenRight = show ")"
    show TokenSquareLeft = show "["
    show TokenSquareRight = show "]"
    show TokenComma = show ","
    show TokenEof = "end of file"

data Token = Token {tokenType :: TokenType, line :: Int} deriving Show

type Tokens = [Token]