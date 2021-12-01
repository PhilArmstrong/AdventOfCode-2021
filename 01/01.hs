import Data.List (foldl')

main = do
  input <- getContents
  let depths = (map read $ lines input)::[Int]
  -- count values that are larger than previous value
  let val1 = foldl' (\ (prev,count) next -> if next > prev
                                            then (next,count+1)
                                            else (next,count)) (head depths,0) (tail depths)
  -- count sliding window sums that are larger than the previous one. (Window size is 3)
  let val2 = foldl' (\ ((a,b,c), count) next -> if (b+c+next) > (a+b+c)
                                                then ((b,c,next),count+1)
                                                else ((b,c,next),count)) (top3, 0) rest
        where
          one:two:three:rest = depths
          top3 = (one,two,three)
  print val1
  print val2
