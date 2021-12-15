import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Char
import qualified Data.Vector.Unboxed as U
import Data.Heap (Heap, Entry)
import qualified Data.Heap as H
import Debug.Trace

data Grid = G Int Int (U.Vector Int)

main = do
  input <- getContents
  let ls = lines input
      nums = map (map digitToInt) ls
      x = length $ head ls
      y = length ls
      h (x1,y1) = x-x1+y-y1
      matrixall = U.fromList $ concat nums
      grid = G x y matrixall
      part1 = a_star grid (x,y) h glookup
      part2 = a_star grid (5*x,5*y) h glookup2
  putStrLn $ "Part 1: " ++ show (sum $ map (glookup grid) (tail $ reverse part1))
  putStrLn $ "Part 2: " ++ show (sum $ map (glookup2 grid) (tail $ reverse part2))

-- It’s A* time!

glookup :: Grid -> (Int,Int) -> Int
glookup (G x y vec) (x1,y1) = vec U.! (y1*x + x1)

glookup2 :: Grid -> (Int,Int) -> Int
glookup2 (G x y vec) (x1,y1) = (( (vec U.! (dy*x + dx)) + addy + addx - 1) `mod` 9) +1
  where
    (addy,dy) = y1 `divMod` y
    (addx,dx) = x1 `divMod` x

neighbours :: (Int,Int) -> (Int, Int) -> [(Int, Int)]
neighbours (x1,y1) (x,y) = filter (not . (\(x,y) -> x<0 || y<0 || x>=x1 || y>=y1)) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

reconstructPath :: Map (Int,Int) (Int,Int) -> (Int,Int) -> [(Int,Int)]
reconstructPath cameFrom current = case M.lookup current cameFrom of
  Just pos -> pos:reconstructPath cameFrom pos
  Nothing -> []

a_star :: Grid -> (Int,Int) -> ((Int, Int) -> Int) -> (Grid->(Int,Int)->Int) -> [(Int,Int)]
a_star grid (sizex,sizey) heuristic lu =
  let
    goal = (sizex-1,sizey-1)
    
    loop :: Heap (H.Entry Int (Int,Int)) -> Set (Int,Int) -> Map (Int,Int) (Int,Int) -> Map (Int,Int) Int -> [(Int,Int)]
    loop heap workingSet cameFrom gScore
      | H.null heap  = error "Never reached goal, but exhausted nodeSet"
      | H.payload (H.minimum heap) == goal = goal:reconstructPath cameFrom goal
      | otherwise = loop openSet'' workingSet'' cameFrom' gScore'
          where
            current  = H.payload $ H.minimum heap
            openSet' = H.deleteMin heap
            workingSet' = S.delete current workingSet
            currentScore = fromMaybe 10000 (M.lookup current gScore)
            ns = neighbours (sizex,sizey) current
            candidate_gScores = map (\pos -> currentScore + lu grid pos) ns

            newScores [] o w c g = (o,w,c,g)
            newScores ((pos,s):ss) o w c g =
              if s < fromMaybe 10000 (M.lookup pos gScore)
              then
                newScores ss (if S.member pos workingSet' then o else H.insert (H.Entry (s+heuristic pos) pos) o)
                 (S.insert pos w) (M.insert pos current c) (M.insert pos s g)
                
              else
                newScores ss o w c g

            (openSet'',workingSet'',cameFrom',gScore') = newScores (zip ns candidate_gScores) openSet' workingSet' cameFrom gScore
  in
    loop (H.singleton (H.Entry (heuristic (0,0)) (0,0))) (S.singleton (0,0)) M.empty (M.singleton (0,0) 0)
