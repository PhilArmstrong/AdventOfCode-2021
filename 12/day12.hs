
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Char
import Data.Graph (edges)
import Debug.Trace

main = do
  input <- getContents
  let ls = lines input
      dat = map (splitOn "-") ls
      edges = M.fromAscListWith (++)
        (sort $ map (\[x,y] -> (x,[y]))
          (filter (\[x,y] -> x/= "end" && y /= "start") $ dat ++ map reverse dat))
  let paths1 = generate1 edges
  print $ "Part 1: " ++ show (length paths1)
  let paths2 = generate2 edges
  print $ "Part 2: " ++ show (length paths2)

generate1 :: M.Map String [String] -> [[String]]
generate1 edges = 
  let
    loop :: [String] -> String -> [[String]]
    loop seen "end" = [["end"]]
    loop seen node = case M.lookup node edges of
      Nothing -> error "Unknown node"
      Just ss -> map (node:) $ concatMap (loop seen') next
        where
          seen' = if all isLower node then node:seen else seen
          next = ss \\ seen

  in loop [] "start"

generate2 :: M.Map String [String] -> [[String]]
generate2 edges = 
  let
    caves = map Just $ filter (\x -> all isLower x && x /= "end" && x/="start") $ M.keys edges

    loop :: [String] -> Maybe String -> String -> [[String]]
    loop seen _    "end" = [["end"]]
    loop seen twice node = case M.lookup node edges of
      Nothing -> error "Unknown node"
      Just ss -> map (node:) $ concatMap (loop seen' twice') next
        where
          seen' = if all isLower node
                  then (case twice of
                           Nothing -> node:seen
                           Just s -> if s==node then seen else node:seen)
                  else seen
          next = ss \\ seen
          twice' = case twice of
            Nothing -> Nothing
            Just s -> if s==node then Nothing else Just s
              
  in map head $ group $ sort $ concatMap ((\x -> x "start") . loop []) caves

