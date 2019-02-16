module Linker where
import Data.Word (Word8)
import Data.Int
import Data.Bits
import Data.Map (Map, lookup)

import Utils (toBin)
import Error (Error (..))
import Asm (ArchWord (..))

data Bins = Bins [Int32]

instance Show Bins where
    show (Bins []) = []
    show (Bins (bin:bins)) =
        (toBin (fromIntegral bin) 31) ++ "\n" ++ (show $ Bins bins)

link :: ([ArchWord], Map String Int) -> Either Error Bins
link ([], _) = Right (Bins [])
link ((Direct bin):words, symbols) = do
    Bins bins <- link (words, symbols)
    return $ Bins $ bin:bins
link ((Waiting sym):words, symbols) = case Data.Map.lookup sym symbols of
    Just bin -> do
        Bins bins <- link (words, symbols)
        return $ Bins $ (fromIntegral bin):bins
    Nothing -> Left $ ErrorUndefSymbol sym

asBytes :: Bins -> [Word8]
asBytes (Bins []) = []
asBytes (Bins (x:xs)) = fmap fromIntegral [
        x `shiftR` 24,
        x `shiftR` 16,
        x `shiftR` 8,
        x
    ] ++ asBytes (Bins xs)