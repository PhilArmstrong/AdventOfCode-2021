import Data.List (foldl')

main = do
  input <- getContents
  let depths = (map read $ lines input)::[Int]
  -- count values that are larger than previous value / window of 3
  let vals = map (\i -> sum $ map fromEnum $ zipWith (<) depths (drop i depths)) [1,3]
  print vals
