module Parser where
import Error (Error (..))
import Token (TokenType (..), Token (..), Tokens)
import Stmt (Expr (..), Exprs, Stmt (..), Stmts)


data Parser = Parser [Token] deriving Show

-- expr from token single (one input)
exprFromTokenS :: Token -> Maybe (Exprs -> Expr)
exprFromTokenS (Token TokenNot _) = Just $ \a -> ExprNot a
exprFromTokenS (Token TokenSub _) = Just $ \a -> ExprNeg a
exprFromTokenS _ = Nothing

-- expr from token double (two inputs)
exprFromTokenD :: TokenType -> Maybe (Exprs -> Exprs -> Expr)
exprFromTokenD TokenOr = Just ExprOr
exprFromTokenD TokenAnd = Just ExprAnd
exprFromTokenD TokenXor = Just ExprXor
exprFromTokenD TokenEq = Just ExprEq
exprFromTokenD TokenLt = Just ExprLt
exprFromTokenD TokenGt = Just ExprGt
exprFromTokenD TokenAdd = Just ExprAdd
exprFromTokenD TokenSub = Just ExprSub
exprFromTokenD TokenMul = Just ExprMul
exprFromTokenD TokenDiv = Just ExprDiv
exprFromTokenD _ = Nothing

getUnit :: Parser -> Either Error (Parser, Exprs)
-- literals
getUnit (Parser ((Token TokenEmpty _):tokens)) = Right (Parser tokens, [ExprEmpty])
getUnit (Parser ((Token (TokenNum n) _):tokens)) = Right (Parser tokens, [ExprNum n])
getUnit (Parser ((Token (TokenReg r) _):tokens)) = Right (Parser tokens, [ExprReg r])
getUnit (Parser ((Token (TokenIden s) _):tokens)) = Right (Parser tokens, [ExprIden s])
-- grouping
getUnit (Parser ((Token TokenParenLeft _):tokens)) = do
    (parser, exprs) <- getExprs (Parser tokens)
    case parser of
        Parser ((Token TokenParenRight _):tokens) -> Right (Parser tokens, exprs)
        Parser (token:tokens) -> Left $ ErrorExpectParenRight token
getUnit (Parser ((Token TokenSquareLeft _):tokens)) = do
    (parser, exprs) <- getExprs (Parser tokens)
    case parser of
        Parser ((Token TokenSquareRight _):tokens) -> Right (Parser tokens, [ExprMem exprs])
        Parser (token:tokens) -> Left $ ErrorExpectParenRight token
-- unary operators and error
getUnit (Parser (token:tokens)) = case exprFromTokenS token of
    Just con -> do
        (parser, exprs) <- getUnit (Parser tokens)
        return (parser, [con exprs])
    Nothing -> Left $ ErrorExpectUnit token

getExprWith :: Parser -> Exprs -> Either Error (Parser, Exprs)
getExprWith (Parser ((Token tokenType line):tokens)) exprs0 = case exprFromTokenD tokenType of
    Just con -> do
        (parser, exprs1) <- getUnit (Parser tokens)
        getExprWith parser [(con exprs0 exprs1)]
    Nothing -> Right $ (Parser ((Token tokenType line):tokens), exprs0)

getExpr :: Parser -> Either Error (Parser, Exprs)
getExpr parser = do
    (parser, exprs) <- getUnit parser
    getExprWith parser exprs

getExprsWith :: Parser -> Exprs -> Either Error (Parser, Exprs)
getExprsWith (Parser ((Token TokenComma _):tokens)) exprs0 = do
    (parser, exprs1) <- getExpr (Parser tokens)
    getExprsWith parser (exprs0 ++ exprs1)
getExprsWith parser exprs = Right (parser, exprs)

getExprs :: Parser -> Either Error (Parser, Exprs)
getExprs parser = do
    (parser, exprs) <- getExpr parser
    getExprsWith parser exprs

-- gets assignment statement or immediate
getSet :: Parser -> Either Error (Parser, Stmt)
getSet parser = do
    (parser, exprs0) <- getExprs parser
    case parser of
        Parser ((Token TokenTo _):tokens) -> do
            (parser, exprs1) <- getExprs (Parser tokens)
            case parser of
                Parser ((Token TokenCond _):tokens) -> do
                    (parser, exprs2) <- getExprs (Parser tokens)
                    return (parser, StmtSet exprs0 exprs1 exprs2)
                _ -> Right (parser, StmtSet exprs0 exprs1 [ExprTrue])
        Parser (token:tokens) -> case exprs0 of
            [ExprNum n] -> Right (Parser (token:tokens), StmtNum n)
            [ExprIden s] -> Right (Parser (token:tokens), StmtIden s)
            _ -> Left $ ErrorExpectStmt token

getStmt :: Parser -> Either Error (Parser, Stmt)
getStmt (Parser ((Token (TokenIden s) _):tokens)) =
    Right (Parser tokens, StmtLabel s)
getStmt (Parser ((Token TokenTab _):tokens)) = getSet (Parser tokens)
getStmt (Parser (token:tokens)) = Left $ ErrorExpectStmt token

getStmts :: Parser -> Either Error Stmts
getStmts (Parser [Token TokenEof _]) = Right []
getStmts (Parser ((Token TokenNewline _):tokens)) = getStmts (Parser tokens)
getStmts parser = do
    (parser, stmt) <- getStmt parser
    case parser of
        Parser ((Token TokenNewline _):tokens) -> do
            stmts <- getStmts (Parser tokens)
            return $ stmt:stmts
        Parser [Token TokenEof _] -> do
            return [stmt]
        Parser (token:tokens) -> Left $ ErrorExpectNewline token