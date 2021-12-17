import Data.Maybe
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Numeric (readInt, readHex, showHex, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Text.Printf
import Data.Either
import Linear.V2
import Control.Arrow

data Probe = Probe { position :: (Int,Int)
                   , velocity :: (Int,Int) } deriving (Show)

target = (V2 150 (-136), V2 193 (-86))

testPos p = x>=150 && x<=193 && y>=(-136) && y<=(-86)
  where (x,y) = position p

main :: IO ()
main = do
  let paths = [path (Probe {position=(0,0), velocity=(dx,dy)})  | dx <- [0..200],  dy <- [(-200)..200]]
  let validpaths = filter (or . (map testPos)) paths  
  let heights = map (maximum . map (snd . position)) validpaths
  let max = maximum heights
  print max
  print $ length validpaths
path :: Probe -> [Probe]
path p = takeWhile (\Probe {position=(x, y),velocity=(dx, dy)} -> y >= (-136)) $ iterate step p

step :: {-(position,velocity)-} Probe -> Probe
step Probe {position=(x,y),velocity=(dx,dy)} = Probe { position = (x+dx,y+dy), velocity = v'}
     where
       v' = (dx', dy-1)
       dx' = case compare dx 0 of
         LT -> dx + 1
         EQ -> 0
         GT -> dx - 1

