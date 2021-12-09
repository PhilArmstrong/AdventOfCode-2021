import Data.List
import Data.List.Split
import Data.Array
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Maybe
import Data.Either
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
  let result = solve nums' boardst
  putStrLn $ "Part1: "++show (head result)
  putStrLn $ "Part2: "++show (last result)

solve :: [Int] -> [Board] -> [Int]
solve [] boards = []
solve (n:ns) boards = let
  newboards = map (filterBoard n) boards
  checkBoards = map checkBoard newboards
  in
    map (n*) (lefts checkBoards) ++ solve ns (rights checkBoards)

checkBoard :: Board -> Either Int Board
checkBoard (x,xt) =
  if elem [] x || elem [] xt
  then
    Left $ sum (map sum x)
  else
    Right (x,xt)

filterBoard :: Int -> Board -> Board
filterBoard n (x,xt) = (map (filter (/= n)) x, map (filter (/= n)) xt)
