module Error where
import Token (Token (..))
import Stmt (Expr, Exprs, Stmt)

data Error = ErrorInvalidChar Char Int -- character, line
    | ErrorExpectUnit Token
    | ErrorExpectStmt Token
    | ErrorExpectNewline Token
    | ErrorExpectParenRight Token
    | ErrorInvalidInst Stmt
    | ErrorInvalidCond Exprs
    | ErrorInvalidOperand Exprs
    | ErrorUndefSymbol String

instance Show Error where
    show (ErrorInvalidChar char line) = "line " ++ (show line) ++ ": unexpected character " ++ (show [char])
    show (ErrorExpectUnit token) = "line " ++ (show $ line token) ++ ": expected an operand (num, iden, reg, mem) e.g.\n\t* 123\n\t* foo\n\t* r4\n\t* [r4]\n\tgot " ++ show (tokenType token)
    show (ErrorExpectStmt token) = "line " ++ (show $ line token) ++ ": expected a statement e.g.\n\t* label \\n\n\t* \\t num | iden | reg | mem, ... -> oper \\n\n\t* num | iden \\n\n\tgot " ++ show (tokenType token)
    show (ErrorExpectNewline token) = "line " ++ (show $ line token) ++ ": expected a newline to end a statement, got " ++ show (tokenType token)
    show (ErrorExpectParenRight token) = "line " ++ (show $ line token) ++ ": expected \"(\" to be followed up with \")\" e.g.\n\t* (a + b)\n\t* (a, b, c)\n\tgot " ++ show (tokenType token)
    show (ErrorInvalidInst stmt) = "line " ++ "todo" ++ ": expected a valid instruction (too many to list) e.g.\n\t* r0 -> r1\n\t* r0, r1 -> r1, r0\n\t* r0, r1 -> r0 + r1\n\tgot " ++ show stmt
    show _ = "todo"