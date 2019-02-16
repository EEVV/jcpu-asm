module Asm where
import Debug.Trace


import Data.Int
import Data.Bits
import Data.Map (Map, empty, insert)

import Utils (toBin)
import Error (Error (..))
import Stmt (Expr (..), Exprs, Stmt (..), Stmts)

data ArchWord = Direct Int32 | Waiting String

instance Show ArchWord where
    show (Direct n) = toBin (fromIntegral n) 31
    show (Waiting s) = "Waiting " ++ s

data Asm = Asm {
    stmts :: Stmts,
    symbols :: Map String Int,
    offset :: Int
}

data Inst = Inst {
    i0 :: Bool,
    i1 :: Bool,
    w0 :: Bool,
    w1 :: Bool,
    src0 :: Int, -- [0, 16)
    src1 :: Int, -- [0, 16)
    dest0 :: Int, -- [0, 16)
    dest1 :: Int, -- [0, 16)
    cond :: Int, -- [0, 16)
    ci :: Bool,
    ce :: Bool,
    opcode :: Int,
    imm0 :: ArchWord,
    imm1 :: ArchWord
}

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

instGen :: Inst -> [ArchWord]
instGen inst = [Direct $ fromIntegral $
        (fromBool (i0 inst)) .|.
        (fromBool (i1 inst)) `shift` 1 .|.
        (fromBool (w0 inst)) `shift` 2 .|.
        (fromBool (w1 inst)) `shift` 3 .|.
        (src0 inst) `shift` 4 .|.
        (src1 inst) `shift` 8 .|.
        (dest0 inst) `shift` 12 .|.
        (dest1 inst) `shift` 16 .|.
        (cond inst) `shift` 20 .|.
        (fromBool (ci inst)) `shift` 24 .|.
        (fromBool (ce inst)) `shift` 25 .|.
        (opcode inst) `shift` 26
    ] ++ (if i0 inst
        then [imm0 inst]
        else [])
    ++ (if i1 inst
        then [imm1 inst]
        else [])
    

asmInstCond :: Exprs -> Either Error Inst
asmInstCond [ExprTrue] = Right $ Inst
    False
    False
    False
    False
    0
    0
    0
    0
    0
    False
    False
    0
    (Waiting [])
    (Waiting [])
asmInstCond [ExprEq [ExprReg r] [ExprNum 0]] = Right $ Inst
    False
    False
    False
    False
    0
    0
    0
    0
    r
    False
    True
    0
    (Waiting [])
    (Waiting [])
asmInstCond [ExprNot [ExprEq [ExprReg r] [ExprNum 0]]] = Right $ Inst
    False
    False
    False
    False
    0
    0
    0
    0
    r
    True
    True
    0
    (Waiting [])
    (Waiting [])
asmInstCond exprs = Left $ ErrorInvalidCond exprs

asmInstOps :: Inst -> (Expr, Expr) -> (Expr, Expr) -> Either Error Inst
asmInstOps inst (dest1, dest0) (src1, src0) = do
    inst <- case src0 of
        ExprNum n -> Right $ inst {i0 = True, imm0 = Direct $ fromIntegral n}
        ExprIden s -> Right $ inst {i0 = True, imm0 = Waiting s}
        ExprReg r -> Right $ inst {src0 = r}
        ExprEmpty -> Right inst
        _ -> Left $ ErrorInvalidOperand [src0]
    inst <- case src1 of
        ExprNum n -> Right $ inst {i1 = True, imm1 = Direct $ fromIntegral n}
        ExprIden s -> Right $ inst {i1 = True, imm1 = Waiting s}
        ExprReg r -> Right $ inst {src1 = r}
        ExprEmpty -> Right inst
        _ -> Left $ ErrorInvalidOperand [src1]
    inst <- case dest0 of
        ExprReg r -> Right $ inst {w0 = True, dest0 = r}
        ExprEmpty -> Right inst
    inst <- case dest1 of
        ExprReg r -> Right $ inst {w1 = True, dest1 = r}
        ExprEmpty -> Right inst
    return inst

-- expects [Expr] to contain only one element
onlySingle :: Exprs -> Either Error Expr
onlySingle [expr] = Right expr
onlySingle exprs = Left $ ErrorInvalidOperand exprs

-- expects ([Expr], [Expr]) to contain one element each
onlySingles :: (Exprs, Exprs) -> Either Error (Expr, Expr)
onlySingles (exprs0, exprs1) = do
    expr0 <- onlySingle exprs0
    expr1 <- onlySingle exprs1
    return (expr0, expr1)

asmInst :: Asm -> Either Error (Asm, [ArchWord])
-- reduction
-- TODO
-- label
asmInst (Asm ((StmtLabel s):stmts) symbols offset) =
    Right (Asm stmts (insert s offset symbols) offset, [])
-- literals
asmInst (Asm ((StmtNum n):stmts) symbols offset) =
    Right (Asm stmts symbols offset, [Direct $ fromIntegral n])
asmInst (Asm ((StmtIden s):stmts) symbols offset) =
    Right (Asm stmts symbols offset, [Waiting s])
-- general instructions
asmInst asm = case stmts asm of
    (StmtSet exprs0 exprs1 cond):stmts -> do
        inst <- asmInstCond cond
        -- srcs has the possibility to change the instruction
        -- from `mov` to other
        (inst, srcs) <- case exprs1 of
            -- mov operations
            [ExprNum n] -> Right (inst, (ExprEmpty, ExprNum n))
            [ExprNum n0, ExprNum n1] -> Right (inst, (ExprNum n0, ExprNum n1))
            [ExprIden s] -> Right (inst, (ExprEmpty, ExprIden s))
            [ExprIden s0, ExprIden s1] -> Right (inst, (ExprIden s0, ExprIden s1))
            [ExprReg r] -> Right (inst, (ExprEmpty, ExprReg r))
            [ExprReg r0, ExprReg r1] -> Right (inst, (ExprReg r0, ExprReg r1))
            -- operations
            -- not
            [ExprNot [expr]] -> do
                return (inst {opcode = 1}, (ExprEmpty, expr))
            -- or
            [ExprOr exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 2}, srcs)
            -- and
            [ExprAnd exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 4}, srcs)
            -- xor
            [ExprXor exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 6}, srcs)
            -- add
            [ExprAdd exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 9}, srcs)
            -- sub
            [ExprSub exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 10}, srcs)
            -- mul
            [ExprMul exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 20}, srcs)
            -- div
            [ExprDiv exprs0 exprs1] -> do
                srcs <- onlySingles (exprs0, exprs1)
                return (inst {opcode = 21}, srcs)
            exprs -> Left $ ErrorInvalidInst (StmtSet exprs0 exprs1 cond)
        dests <- case exprs0 of
            [ExprReg r] -> Right (ExprEmpty, ExprReg r)
            [ExprReg r0, ExprReg r1] -> Right (ExprReg r0, ExprReg r1)
        inst <- asmInstOps inst dests srcs
        return (asm {stmts = stmts}, instGen inst)

assemble :: Asm -> Either Error ([ArchWord], Map String Int)
assemble (Asm [] symbols _) = Right ([], symbols)
assemble asm = do
    (asm, words0) <- asmInst asm
    (words1, symbols) <- assemble (asm {offset = offset asm + length words0})
    return (words0 ++ words1, symbols)



