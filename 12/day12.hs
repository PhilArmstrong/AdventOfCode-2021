
import Data.List
import Data.List.Split

main = do
  input <- getContents
  let ls = lines input
      dat = map (splitOn "-") ls
  print dat



