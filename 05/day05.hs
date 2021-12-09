import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Maybe
import Data.Either ()
import Linear.V2

type Coord = V2 Int

main = do
  input <- getContents
  let lines = parse input
  print $ filter valid1 lines
  let p1 = countIntersects M.empty $ filter valid1 lines
  let p2 = countIntersects M.empty lines
  print p1
  print p2

valid1 :: (Coord, Coord) -> Bool
valid1 (V2 x1 y1, V2 x2 y2) = x1==x2 || y1==y2
  
parse :: String -> [(Coord,Coord)]
parse input = let
  ls = traceShowId $ map words $ lines input
  in
    map parseLine ls

parseLine :: [String] -> (Coord,Coord)
parseLine [start,_,end] = (startc,endc)
  where
    startc = V2 (start'!!0) (start'!!1)
    endc = V2 (end'!!0) (end'!!1)
    start' = getPair start
    end' = getPair end
    getPair n = map read $ splitOn "," n
parseLine p = error "bad line" 

countIntersects :: M.Map Coord Int -> [(Coord,Coord)] -> Int
countIntersects grid [] = trace (showGrid grid) length $ filter (>=2) $ map snd $ M.toList grid
countIntersects grid ((V2 x1 y1, V2 x2 y2):cs) = let
  coords | x1==x2 = [V2 x1 y | y <- [y1,y1+signum (y2-y1)..y2]]
         | y1==y2 = [V2 x y1 | x <- [x1,x1+signum (x2-x1)..x2]]
         | otherwise = [V2 (x1 + o*(signum (x2-x1)))  (y1 + o*(signum (y2-y1))) | o <- [0..abs (x2-x1)]]

  buildGrid g []     = g
  buildGrid g (l:ls) = buildGrid (M.insertWith (+) l 1 g) ls
  in
    countIntersects (buildGrid grid coords) cs

showGrid :: M.Map Coord Int -> String
showGrid grid = show $ intercalate "\n" $ do
  y <- [0..9]
  return $ do
    x <- [0..9]
    show $ M.findWithDefault 0 (V2 x y) grid
  
  
