import Data.List.Split
import Data.List
import GHC.Float

main = do
  input <- getContents
  let vals = (map read $ splitOn "," input)::[Int]
  let rms = sqrt (fromIntegral (sum $ map (^2) vals)) / fromIntegral (length vals)
  let median = sort vals !! div (length vals) 2
  let mean = (fromIntegral (sum vals)) / (fromIntegral (length vals))
  let low = (floor mean)::Int
  let high = (ceiling mean)::Int
  putStrLn $ "Mean: " ++ show mean ++ " Median: " ++ show median
  putStrLn $ "Part 1: " ++ (show $ sum $ map (\x -> abs (x - median)) vals)  
  putStrLn $ "Part 2: " ++ (show $ sum $ map (\x -> sum' $ abs (x - low)) vals)
  putStrLn $ "Part 2: " ++ (show $ sum $ map (\x -> sum' $ abs (x - high)) vals)

sum' n = div (n^2 + n) 2
