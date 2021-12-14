import Data.List
import Data.List.Split
import qualified Control.Foldl as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import GHC.Float (fabsFloat)

main = do
  input <- getContents
  let ls = lines input
      t  = head ls
      ps = M.fromList $ map ((\[[i1,i2],_,[y]] -> ((i1,i2),y)) . words) $ drop 2 ls
  let steps = iterate (step1 ps) t
      ten = steps !! 10
      lens = sort $ map length $ group $ sort ten
  let steps2 = iterate (step2 ps) (countPairs (t++"Z")) -- add a terminal char
  
  putStrLn $ "Part 1: " ++ show (last lens - head lens)
  let counts = sort $ map snd $ M.toList $ countChars $ steps2 !! 40
  putStrLn $ "Part 2: " ++ show (last counts - head counts)

-- Iterative calculation
step1 ::  M.Map (Char,Char) Char -> String -> String
step1 rules input =
  let
    loop s [i] = reverse (i:s)
    loop s (i1:i2:is) = case M.lookup (i1,i2) rules of
      Just c -> loop (c:i1:s) (i2:is)
      Nothing -> loop (i1:s) (i2:is)
    loop s [] = reverse s
  in
    loop "" input
                          

-- direct method for Part 2! Could probably do this with Matrix multiplication of the adjacency
-- matrix between pairs in a linear algebra library, but this method is fast enough.
countPairs :: String -> M.Map (Char,Char) Int
countPairs is = let
  loop [] m = m
  loop [i1] m = m
  loop (i1:i2:s) m = loop (i2:s) $ M.insertWith (+) (i1,i2) 1 m
  in
    loop is M.empty

countChars :: M.Map (Char,Char) Int -> M.Map Char Int
countChars m = let
  loop [] newm = newm
  loop (((char,_),count):ss) newm = loop ss $ M.insertWith (+) char count newm
  in
    loop (M.toList m) M.empty

step2 :: M.Map (Char,Char) Char -> M.Map (Char,Char) Int -> M.Map (Char,Char) Int
step2 rules m =
  let
    loop [] newm = newm
    loop ((str,count):ss) newm = loop ss $ M.unionWith (+) newm $ M.fromList newstr
      where
        (i1,i2) = str
        newstr = case M.lookup str rules of
          Nothing -> [(str, count)]
          Just i -> [((i1,i),count),((i,i2),count)]
  in
    loop (M.toList m) M.empty
