import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Char
import Data.Maybe ( mapMaybe, fromJust, catMaybes )
import Control.Monad
import Data.List
import Debug.Trace

data Matrix a = Matrix Int Int (U.Vector a) deriving Show

main = do
  input <- getContents
  let ls = map (map digitToInt) $ lines input
      x = length $ head ls
      y = length ls
      matrixall = U.fromList $ concat ls
      matrix = Matrix x y matrixall
  print matrix
  putStrLn $ "Part 1: " ++ show (sum $ map fst (take 100 $ steps matrix))
  putStrLn $ "Part 2: " ++ show ((+1) <$> (elemIndex 100 $ map fst (steps matrix)))

-- Ugh!
steps m = (c,newm):steps newm 
  where (c,newm) = step m

-- Ugh!!
step :: Matrix Int -> (Int,Matrix Int)
step m@(Matrix w h vec) = let
  -- add one to all the octopodes
  plusvec = U.map (+1) vec
  -- flashing?
  flashes :: Matrix Int -> [(Int,Int)]
  flashes m@(Matrix w h _) =
    [(x,y) | x<-[0..w-1], y<-[0..h-1], (fromJust $ lookupMat m (x,y)) > 9 ]
  -- Find the set of flashing octopodes
  firstflashes = flashes (Matrix w h plusvec)
  -- Set all of those to -1 to mark them as flashed
  firstflashvec = plusvec U.// zip (map (\(x,y) -> y*w+x) firstflashes) (repeat (negate 1))

  -- iterate until we have no more flashing octopodes
  loop :: [(Int,Int)] -> [(Int,Int)] -> U.Vector Int -> (Int,Matrix Int)
  loop [] seen octopodes = (length seen, Matrix w h (U.map (\x -> if x==negate 1 then 0 else x) octopodes))
  loop (f:lashes) seen octopodes = let
    validns = map (\(x,y) -> y*w+x) $ filter (\(x,y) -> x>=0 && x<w && y >=0 && y < h) $ neighbours f
    plusoctopodes = U.accum (\o _ -> if o/=negate 1 then o+1 else negate 1) octopodes (zip validns (repeat 1))
    newflashes = flashes (Matrix w h plusoctopodes) \\ seen
    newflashvec = plusoctopodes U.// zip (map (\(x,y) -> y*w+x) newflashes) (repeat (negate 1))
    in
      loop (newflashes++lashes) (f:seen) newflashvec
  in
    loop firstflashes [] firstflashvec
    
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x+dx,y+dy) | dx <-[-1..1], dy <- [-1..1], not (dx==0 && dy==0)]

lookupMat :: U.Unbox a => Matrix a -> (Int,Int) -> Maybe a
lookupMat (Matrix w _ vec) (x,y) = vec U.!? (w*y+x)
                 
                 
  
