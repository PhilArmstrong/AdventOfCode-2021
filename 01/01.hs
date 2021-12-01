import Data.List (foldl')

main = do
  input <- getContents
  let depths = (map read $ lines input)::[Int]
  -- count values that are larger than previous value / window of 3
  let vals = map (\i -> count $ zipWith (<) depths (drop i depths)) [1,3]
  print vals

count :: (Foldable t1, Num a) => t1 Bool -> a
count = foldl' (\a x -> if x then a+1 else a) 0

countp :: (Foldable t1, Num a) => (b -> Bool) -> t1 b -> a
countp p = foldl' (\a x -> if p x then a+1 else a) 0
