import Data.List (foldl')

main = do
  input <- getContents
  let depths = (map read $ lines input)::[Int]
  -- count values that are larger than previous value
  let val1 = count (uncurry (<)) (zip depths (drop 1 depths))
  -- count sliding window sums that are larger than the previous one. (Window size is 3)
  let val2 = foldl' (\ ((a,b,c), count) next -> if (b+c+next) > (a+b+c)
                                                then ((b,c,next),count+1)
                                                else ((b,c,next),count)) (top3, 0) rest
        where
          one:two:three:rest = depths
          top3 = (one,two,three)
  print val1
  print val2

count p = foldl' (\a x -> if p x then a+1 else a) 0

