import Data.List (foldl')
import TcRnMonad (TcPatSynInfo(patsig_ex_bndrs))

main = do
  input <- getContents
  let depths = (map read $ lines input)::[Int]
  -- count values that are larger than previous value
  let val1 = count $ zipWith (<) vals (drop 1 vals) where vals = window 1 depths -- (tail depths)
  -- count sliding window sums that are larger than the previous one. (Window size is 3)
  let val2 = count $ zipWith (<) vals (drop 1 vals) where vals = window 3 depths
  print val1
  print val2

count :: (Foldable t1, Num a) => t1 Bool -> a
count = foldl' (\a x -> if x then a+1 else a) 0

window :: Num a => Int -> [a] -> [a]
window n input = scanl (\ window (prev,next) -> window+next-prev) initial $ zip input (drop n input)
  where
    initial = sum (take n input)
