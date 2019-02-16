module Utils where
import Data.Bits

toBin :: Int -> Int -> String
toBin a 0
    | testBit a 0 = "1"
    | otherwise = "0"
toBin a n = toBin (shift a (-1)) (n - 1) ++ toBin a 0