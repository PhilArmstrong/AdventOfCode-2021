import Data.Maybe
import Debug.Trace
import Data.List.Split
import Numeric (readInt, readHex, showHex, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Text.Printf
import Data.Either
import qualified Data.Vector.Unboxed as U
import Linear.V3
import Linear.Quaternion
import Data.Bool
import Control.Monad
import qualified Data.Vector.Algorithms.Intro as V (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

main :: IO ()
main = do
  input <- getContents
  let ls = map (map (\x -> "("++x++")") . tail . lines) $ splitOn "\n\n" input
  let scanners = map (map (\(x,y,z) -> V3 x y z)) ((map (map read) ls)::[[(Int,Int,Int)]])
  print scanners
  print $ solve scanners
{-
Place the first scanner at 0,0,0

We can use the manhattan distance between the points in a scan as a unifying measure.

-}
manhattan :: V3 Int -> V3 Int -> Int
manhattan a@(V3 x1 y1 z1) b@(V3 x2 y2 z2) =  sum $ abs (a-b) --(abs x1-x2) + (abs y1-y2) + (abs z1-z2)

-- couild 
orientations = [
  id
  ,\(V3 x y z) -> V3 x z (-y)
  ,\(V3 x y z) -> V3 x (-y) (-z)
  ,\(V3 x y z) -> V3 x (-z) y
  ,\(V3 x y z) -> V3 (-x) (-z) y
  ,\(V3 x y z) -> V3 (-x) z y
  ,\(V3 x y z) -> V3 (-x) y (-z)
  ,\(V3 x y z) -> V3 (-x) (-z) (-y)
  ,\(V3 x y z) -> V3 y (-x) z
  ,\(V3 x y z) -> V3 y z x
  ,\(V3 x y z) -> V3 y x (-z)
  ,\(V3 x y z) -> V3 y (-z) (-x)
  ,\(V3 x y z) -> V3 (-y) x z
  ,\(V3 x y z) -> V3 (-y) z (-x)
  ,\(V3 x y z) -> V3 (-y) (-x) (-z)
  ,\(V3 x y z) -> V3 (-y) (-z) x
  ,\(V3 x y z) -> V3 z x y
  ,\(V3 x y z) -> V3 z y (-x)
  ,\(V3 x y z) -> V3 z (-x) (-y)
  ,\(V3 x y z) -> V3 z (-y) x
  ,\(V3 x y z) -> V3 (-z) x (-y)
  ,\(V3 x y z) -> V3 (-z) (-y) (-x)
  ,\(V3 x y z) -> V3 (-z) (-x) y
  ,\(V3 x y z) -> V3 (-z) y x
  ]

solve :: [[V3 Int]] -> Int 
solve scans =
  let
    signatures = map (\ scan -> S.fromAscList $ sort [manhattan x y | (x:xs) <- tails scan, y <- xs]) scans
    
  in
    1
    
