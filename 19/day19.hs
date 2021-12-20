import Data.Maybe
import Debug.Trace
import Data.List.Split
import Numeric (readInt, readHex, showHex, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Text.Printf
import Data.Either
import Linear.V3
import Linear.Quaternion

main :: IO ()
main = do
  input <- getContents
  let ls = map (map (\x -> "("++x++")") . tail . lines) $ splitOn "\n\n" input
  let scanners = map (map (\(x,y,z) -> V3 x y z)) ((map (map read) ls)::[[(Int,Int,Int)]])
  print scanners


diffList :: [Int] -> [Int]
diffList = 

{-
Place the first scanner at 0,0,0

We can 

-}
  
solve ::[[(Int,Int,Int)]] -> Int
solve scanData =
  let
    
