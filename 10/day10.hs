import Data.List
import Data.Maybe
import Data.Map (valid)
import Debug.Trace

main = do
  input <- getContents
  let ls = lines input
  let scores = map (parse1 []) ls
  putStrLn $ "Part1: " ++ show (sum scores)
  let sls = zip scores ls
  let valid = filter (\(x,y) -> x==0) sls
  let incomplete = map snd valid
  let autocomplete = map (parse2 []) incomplete
  putStrLn $ "Part2: " ++ show ((sort autocomplete) !! (div (length autocomplete) 2))

parse1 :: String -> String -> Int
parse1 _ [] = 0 -- out of input
parse1 stack (c:cs) | c `elem` "{[<(" = parse1 (close c:stack) cs
                    | not (null stack) && head stack == c = parse1 (tail stack) cs
                    | c == ']' = 57
                    | c == '}' = 1197
                    | c == ')' = 3
                    | c == '>' = 25137
                    | otherwise = error "unknown character in string"
  where
    close c = "}]>)" !! fromJust (elemIndex c "{[<(")

parse2 :: String -> String -> Int
parse2 stack [] = score 0 stack
parse2 stack (c:cs) | c `elem` "{[<(" = parse2 (close c:stack) cs
                    | not (null stack) && head stack == c = parse2 (tail stack) cs
                    | otherwise = traceShow (stack,c:cs) $ error ("unknown character in string "++[c])
  where
    close c = "}]>)" !! fromJust (elemIndex c "{[<(")
    

score n [] = n
score n (s:ss) = score (n*5 + 1 + fromJust (elemIndex s ")]}>")) ss
