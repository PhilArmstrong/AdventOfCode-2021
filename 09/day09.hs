import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Char
import Data.Maybe ( mapMaybe, fromJust, catMaybes )
import Control.Monad
import Data.List
import Debug.Trace

data Matrix a = Matrix Int Int (V.Vector (U.Vector a)) deriving Show

main = do
  input <- getContents
  let ls = map (map digitToInt) $ lines input
      matrixl = map U.fromList ls
      matrixall = V.fromList matrixl
      matrix = Matrix (U.length $ V.head matrixall) (V.length matrixall) matrixall
  let lowpoints = lows matrix
  putStrLn $ "Part 1: " ++ show (sum $ map ((+1) . fst) lowpoints)
  putStrLn $ "Part 2: " ++ show (product $ take 3 $ reverse $ sort $ map (count matrix . snd) lowpoints)

lows :: Matrix Int -> [(Int,(Int,Int))]
lows m@(Matrix sx sy vec) = catMaybes $ do
  x <- [0..sx-1]
  y <- [0..sy-1]
  do
    let cur = fromJust $ lookupMat m (x,y)
    let vals = mapMaybe (lookupMat m) (neighbours (x,y))
    pure $ if all (cur<) vals then Just (cur,(x,y)) else Nothing

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

lookupMat :: U.Unbox a => Matrix a -> (Int,Int) -> Maybe a
lookupMat (Matrix _ _ vec) (x,y) = vec V.!? y >>= \u -> u U.!? x

count :: Matrix Int -> (Int,Int) -> Int
count m (x,y) = count' m [] [(x,y)]
 where
   count' _ seen [] = length seen
   count' m seen (n:next) = let
     curVal = fromJust $ lookupMat m n
     new = mapMaybe (\p -> case lookupMat m p of
                             Just v -> if v > curVal && v /= 9 then Just p else Nothing
                             Nothing -> Nothing) ((neighbours n \\ seen) \\ next)
     in
       count' m (n:seen) (next++new)
                 
                 
  
