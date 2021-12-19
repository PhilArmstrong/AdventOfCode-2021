import Data.Maybe
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Numeric (readInt, readHex, showHex, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Data.List
import Text.Printf
import Data.Either
import GHC.IO.Handle (NewlineMode(inputNL), nativeNewline)
import Linear (Hamiltonian(_j), Complicated (_e), Metric (dot))

type Parser = Parsec Void String

main :: IO ()
main = do
  input <- getContents
  let nums = lines input
  let snails = map (fromRight (N 0) . parse parsePair "") nums
  let total = foldl1 add snails
  print $ "part 1: " ++ show (magnitude total)
  let magnitudes = sums snails
  print $ maximum magnitudes

data Tree = P Tree Tree | N Int deriving (Read,Show,Eq)

sums :: [Tree] -> [Int]
sums trees =  do
  let l = length trees
  x <- [0..l-1]
  y <- [0..l-1]
  pure $ magnitude $ add (trees!!x) (trees!!y)


parsePair :: Parser Tree
parsePair = do
  char '['
  l <- parsePair <|> (N <$> L.decimal)
  char ','
  r <- parsePair <|> (N <$> L.decimal)
  char ']'
  pure (P l r)

add :: Tree -> Tree -> Tree
add t1 t2 = reduce (P t1 t2)

reduce :: Tree -> Tree
reduce t = case explodeOne t of
  Nothing -> maybe t reduce (splitOne t)
  Just t1 -> reduce t1

-- split leftmost integer
splitOne :: Tree -> Maybe Tree
splitOne (N n) = if n>=10 then Just (P (N n1) (N (n1+r1))) else Nothing where (n1,r1) = n `divMod` 2
splitOne (P l r) = case splitOne l of
                     Nothing -> case splitOne r of
                       Nothing -> Nothing
                       Just r1 -> Just (P l r1)
                     Just l1 -> Just (P l1 r)

explodeOne :: Tree -> Maybe Tree
explodeOne t = case explodeOne' 1 t of
  Nothing -> Nothing
  Just (_,res,_) -> Just res

explodeOne' :: Int -> Tree -> Maybe (Maybe Int,Tree,Maybe Int)
explodeOne' n (N num) = Nothing
explodeOne' n p@(P (N l) (N r)) = if n > 4 then Just (Just l, N 0, Just r) else Nothing
explodeOne' n p@(P l r) = case explodeOne' (n + 1) l of
  Just (Just l1, l', Nothing) -> Just (Just l1, P l' r, Nothing) -- no more lefts to add into, so recurse up
  Just (Nothing, l', Just r1) -> case leftAdd r1 r of
    Nothing -> Just (Nothing, P l' r, Just r1)
    Just r' -> Just (Nothing, P l' r', Nothing)
  Just (Just l1, l', Just r1) -> case leftAdd r1 r of
    Nothing -> Just (Just l1, P l' r, Just r1)
    Just r' -> Just (Just l1, P l' r', Nothing)
  Just (Nothing,l',Nothing) -> Just (Nothing,P l' r,Nothing)
  Nothing                -> case explodeOne' (n + 1) r of
    Nothing               -> Nothing
    Just (Just l1, r', Nothing) -> case rightAdd l1 l of
      Nothing -> Just (Just l1, P l r', Nothing)
      Just l' -> Just (Nothing, P l' r', Nothing)
    Just (Nothing, r', Just r1) -> Just (Nothing, P l r', Just r1) -- no more rights here to add, so recurse up.
    Just (Just l1, r', Just r1) -> case rightAdd l1 l of
      Nothing -> Just (Just l1, P l r', Just r1)
      Just l' -> Just (Nothing, P l' r', Just r1)
    Just (Nothing,r',Nothing) -> Just (Nothing,P l r',Nothing)

rightAdd :: Int -> Tree -> Maybe Tree
rightAdd n (N num) = Just (N (num+n))
rightAdd n (P l r) = case rightAdd n r of
  Just r1 -> Just (P l r1)
  Nothing -> case rightAdd n l of
    Nothing -> Nothing
    Just l1 -> Just (P l1 r)

leftAdd :: Int -> Tree -> Maybe Tree
leftAdd n (N num) = Just (N (num+n))
leftAdd n (P l r) = case leftAdd n l of
  Just l1 -> Just (P l1 r)
  Nothing -> case leftAdd n r of
    Nothing -> Nothing
    Just r1 -> Just (P l r1)

magnitude :: Tree -> Int
magnitude (N n) = n
magnitude (P l r) = 3*magnitude l + 2*magnitude r
