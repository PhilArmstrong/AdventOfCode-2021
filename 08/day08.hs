import Data.List.Split
import Data.List
import Control.Arrow
import qualified Data.Map.Strict as M
import Debug.Trace

main = do
  input <- getContents
  let cases = map (splitOn ["|"] . map sort . words) $ lines input -- cases are a list of pairs 
  print $ part1 0 cases
  print $ head cases
  print $ sum $ map induce cases
  
  --print $ induce $ head cases

part1 n [] = n
part1 n ((l:r:[]):rest) = part1 (n+count r) rest
  where
    count output = length $ filter (\x -> elem (length x) [2,3,4,7]) output
part1 n _ = error "bad input"


induce (l:r:[]) = num $ reverse r
  where
    ln n xs = length xs == n
    one = head $ traceShowId $ filter (ln 2) l
    four = head $ traceShowId $ filter (ln 4) l
    seven = head $ traceShowId $ filter (ln 3) l
    eight = head $ traceShowId $ filter (ln 7) l
    six_nine = traceShowId $ filter (ln 6) l
    five_two_three = traceShowId $ filter (ln 5) l
    (six,nine_zero) = (first head) $ traceShowId $ partition (contains (eight \\ one)) six_nine
    (nine,zero) = (head *** head) $ traceShowId $ partition (contains (four \\ one)) nine_zero
    (three,two_five) = first head $ traceShowId $ partition (contains seven) five_two_three
    (five,two) = (head *** head) $ traceShowId $ partition (contains (four \\ one)) two_five
    numMap = M.fromList [(one,1),(two,2),(three,3),(four,4),(five,5),(six,6),(seven,7),(eight,8),(nine,9),(zero,0)]

    num [] = 0
    num (z:zs) = case M.lookup z numMap of
      Just n -> n + 10 * num zs
      Nothing -> error "Argh!"
induce _ = error "bad induce case"

contains :: (Eq a) => [a] -> [a] -> Bool
contains xs ys = and $ map (\x -> elem x ys) xs
