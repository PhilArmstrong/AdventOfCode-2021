import Data.List
import Data.List.Split
import Data.Array
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Maybe
--newtype Board = Array (Int,Int)
type Board = ([[Int]], [[Int]]) -- second is transpose of the first.

main = do
  input <- getContents
  let nums:boards = splitOn "\n\n" input
  let nums' = (map read $ splitOn "," nums)::[Int]
--  let boards = (map (map read . wordsBy (\x -> x==' ' || x=='\n')) $ tail inputs)::[[Int]]
  let boards' = map (map (map read . words) . (filter (/= []) . splitOn "\n")) boards
  let boardst = zip boards' (map transpose boards')
--  let boardsArray = map (listArray ((0,0),(4,4))) boards
  print $ solve nums' boardst

solve :: [Int] -> [Board] -> Int
solve [] boards = error "No more numbers!"
solve (n:ns) boards = let
  newboards = traceShowId $ map (filterBoard n) boards
  checkBoards = traceShowId $ map checkBoard newboards
  result = catMaybes checkBoards
  in
    case result of
      [] -> solve ns newboards
      x:xs -> traceShow (n,x) $ n*x

checkBoard :: Board -> Maybe Int
checkBoard (x,xt) =
  if elem [] x || elem [] xt
  then
    Just $ sum (map sum x)
  else
    Nothing

filterBoard :: Int -> Board -> Board
filterBoard n (x,xt) = (map (filter (/= n)) x, map (filter (/= n)) xt)
