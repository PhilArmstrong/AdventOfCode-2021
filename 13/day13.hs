import Data.List
import Data.List.Split
import qualified Control.Foldl as L
import qualified Data.Set  as S
import Data.Maybe

main = do
  input <- getContents
  let [pairs, instructions] = splitOn "\n\n" input
      nums = map ((\[x,y] -> (read x, read y)) . splitOn ",") $ lines pairs :: [(Int,Int)]
      instr = (map (\[_,_,str] -> (head str,read $ drop 2 str))
                $ filter (\x -> length x == 3) $ map words $ lines instructions)::[(Char,Int)]
  putStrLn "Part 1: "
  print $ show (length $ filter (=='▒') $ foldover (take 1 instr) nums)
  putStrLn "Part 2: "
  putStrLn (foldover instr nums)

{-
Argument is a list of co-ordinates and a list of fold lines.
Fold the co-ordinates along the fold lines to reveal the final image
-}
foldover :: [(Char,Int)] -> [(Int,Int)] -> String
foldover [] coords = stringify (S.fromAscList $ sort coords)
foldover ((axis,num):olds) coords = foldover olds coords'
  where
   coords' = case axis of
     'x' -> map (\(x,y) -> if x > num then (2*num - x,y) else (x,y)) coords
     'y' -> map (\(x,y) -> if y > num then (x,2*num - y) else (x,y)) coords
     _ -> error "Unexpected item in bagging area."

-- Turn list of coordinates into a string
-- ▒
stringify :: S.Set (Int,Int) -> String
stringify coords = intercalate "\n" $ do
  let (minx, maxx) = L.fold ((,) <$> L.minimum <*> L.maximum) $ map fst (S.toList coords)
      (miny, maxy) = L.fold ((,) <$> L.minimum <*> L.maximum) $ map snd (S.toList coords)
  y <- [fromJust miny .. fromJust maxy]
  pure (do
    x <- [fromJust minx .. fromJust maxx]
    pure
      (if S.member (x,y) coords then '▒' else ' '))
