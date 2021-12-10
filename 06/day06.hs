import Control.Monad
import Data.List.Split
import Data.List
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Typeable

main = do
  input <- getContents
  let fish = (sort $ map read $ splitOn "," input)::[Int]
  vec <- VM.replicate 9 (0::Integer)
  let    loop v [] = return v
         loop v (i:is) = do
           val <- VM.read v i
           VM.write v i (val+1)
           loop v is
  initialVals <- loop vec fish
  f <- V.freeze initialVals
  let step v = do
        front <- VM.read v 0
        let size = VM.length v
            loop v n = if n == 8
                       then
                         do
                           old6 <- VM.read v 6
                           VM.write v 8 front
                           VM.write v 6 (old6 + front)
                           return v
                       else
                         do
                           val <- VM.read v (n+1)
                           VM.write v n val
                           loop v (n+1)
        loop v 0
  let
    iterate 0 v = pure v
    iterate n v = do
        next <- step v
        iterate (n-1) next
        
  final80 <- iterate 80 initialVals
  f80 <- V.freeze final80
  print $ V.sum f80
--  initialVals <- V.thaw f
  final256 <- iterate (256-80) initialVals
  f256 <- V.freeze final256
  print $ V.sum f256







-- Better way - this is a matrix multiplation problem in disguise, so we can use repeated matrix exponentiation to get there quickly.
  
  



  
