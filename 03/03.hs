import Data.List
import Data.Char
import Data.Bits
import Debug.Trace

main = do
  input <- getContents
  let inputNums = map (map digitToInt) $ lines input
  let num = length inputNums
  let (g,e) = ge (countBits inputNums) num
  print ((g,e),g*e)
  let o2' = bitstoNum $ o2 inputNums
  let co2' = bitstoNum $ co2 inputNums
  print ((o2',co2'),o2'*co2')

bitstoNum :: (Show a, Ord a, Num a) => [a] -> Int
bitstoNum ns = bitstoNum' 0 0 (reverse ns)
  where
    bitstoNum' num _ [] = num
    bitstoNum' num b (i:is) = if i>0 then bitstoNum' (setBit num b) (b+1) is else bitstoNum' num (b+1) is

countBits :: [[Int]] -> [Int]
countBits is = map sum $ transpose is

ge :: [Int] -> Int -> (Int, Int)
ge counts l =
  let
    half = div l 2
    g' g e b []     = (g,e)
    g' g e b (c:cs) = if c>half then g' (setBit g b) e (b+1) cs else g' g (setBit e b) (b+1) cs
  in g' 0 0 0 (reverse counts)

o2 :: [[Int]] -> [Int]
o2 input =
  let
    life ::Int -> [[Int]] -> [Int]
    life b is | length is == 1 = head is
              | otherwise      = life (b+1) (filter (\x -> (x!!b) == val) is)
      where
        half = div (length is+1) 2
        val = if (countBits is)!!b >= half then 1 else 0
  in
    life 0 input

co2 :: [[Int]] -> [Int]
co2 input =
  let
    life :: Int -> [[Int]] -> [Int]
    life b is | length is == 1 = head is
              | otherwise      = life (b+1) (filter (\x -> (x!!b) == val) is)
      where
        half = div (length is+1) 2
        val = if (countBits is)!!b < half then 1 else 0
  in
    life 0 input