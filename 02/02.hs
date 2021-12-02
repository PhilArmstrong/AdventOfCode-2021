main = do
  input <- getContents
  let (dist,depth) = parse 0 0 (lines input)
  let (dist2,depth2) = parse2 0 0 0 (lines input)
  print (dist,depth)
  print (dist*depth)
  print (dist2,depth2)
  print (dist2*depth2)

parse :: Int -> Int -> [String] -> (Int,Int)
parse dist depth [] = (dist,depth)
parse dist depth (r:rest) = case command of
  "forward" -> parse (dist+val) depth rest
  "up" -> parse dist (depth-val) rest
  "down" -> parse dist (depth+val) rest
  _ -> error "Unknown command!"
  where
    command = head $ words r
    val = (read $ words r !! 1)::Int

parse2 :: Int -> Int-> Int -> [String] -> (Int,Int)
parse2 aim dist depth [] = (dist,depth)
parse2 aim dist depth (r:rest) = case command of
  "forward" -> parse2 aim (dist+val) (depth+aim*val) rest
  "up" -> parse2 (aim-val) dist depth rest
  "down" -> parse2 (aim+val) dist depth rest
  _ -> error "Unknown command!"
  where
    command = head $ words r
    val = (read $ words r !! 1)::Int
