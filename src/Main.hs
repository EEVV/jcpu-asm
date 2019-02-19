module Main where
import System.Environment
import Data.Map (empty)
import Data.ByteString

import Lexer
import Parser
import Asm
import Linker

{-
command line commands

* jcpu-asm <input> <output>
    saves to output

* jcpu-asm <input>
    displays output as 32 bit a row
-}

printHelp :: IO ()
printHelp = do
    Prelude.putStrLn "usage:\n\tprint binary: jcpu-asm <input file>\n\tsave binary: jcpu-asm <input file> <output file>"

-- gives filenames
inputOutput :: String -> String -> IO ()
inputOutput input output = do
    source <- Prelude.readFile input
    let tokensEither = Lexer.lex source
    let stmtsEither = tokensEither >>= \tokens -> Parser.getStmts (Parser tokens)
    let asmEither = stmtsEither >>= \stmts -> Asm.assemble (Asm stmts Data.Map.empty 0)
    let linkEither = asmEither >>= \asm -> Linker.link asm
    case linkEither of
        Left err -> print err
        Right asm -> do
            Data.ByteString.writeFile output (pack $ Linker.asBytes asm)

inputOnly :: String -> IO ()
inputOnly input = do
    source <- Prelude.readFile input
    let tokensEither = Lexer.lex source
    let stmtsEither = tokensEither >>= \tokens -> Parser.getStmts (Parser tokens)
    case stmtsEither of
        Left err -> return ()
        Right stmts -> print stmtsEither
    let asmEither = stmtsEither >>= \stmts -> Asm.assemble (Asm stmts Data.Map.empty 0)
    let linkEither = asmEither >>= \asm -> Linker.link asm
    case linkEither of
        Left err -> print err
        Right asm -> Prelude.putStr $ show asm

main :: IO ()
main = do
    args <- getArgs
    case Prelude.length args of
        1 -> inputOnly (args !! 0)
        2 -> inputOutput (args !! 0) (args !! 1)
        _ -> printHelp
