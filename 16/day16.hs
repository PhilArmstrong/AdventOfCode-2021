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
import GHC.IO.Handle (NewlineMode(inputNL))

type Parser = Parsec Void String

data Op = Sum | Product | Min | Max | GTo | LTo | EQo deriving Show
data PType = PLit Int | POp Op [Packet] deriving Show
data Packet = P Int {-Version-} PType deriving Show

binToNum :: String -> Int
binToNum = fst . head . readInt 2 (`elem` "01") digitToInt 

parseBin :: Int -> Parser Int
parseBin n = do
  digits <- count n L.charLiteral
  pure $ binToNum digits

parseLiteral :: Parser Int
parseLiteral = binToNum <$> parseLiteralBits

parseLiteralBits :: Parser String
parseLiteralBits = do
  s:digits <- count 5 L.charLiteral
  case s of
    '0' -> return digits
    _ -> do
      rest <- parseLiteralBits
      pure (digits ++ rest)

parseOperator :: Parser [Packet]
parseOperator = do
  lengthTypeID <- L.charLiteral
  case lengthTypeID of
    '0' -> do
      len <- parseBin 15
      beforeInput <- getParserState 
      input <- count len L.charLiteral
      -- store the after parse state
      afterInput <- getParserState
      -- parse the input bitstream for this operator
      setParserState (beforeInput { stateInput = input })
      res <- manyTill parsePacket eof
      afterParse <- getParserState
      -- restore the parse state
      setParserState (afterInput { stateParseErrors = stateParseErrors afterInput ++ stateParseErrors afterParse} )
      pure res
    _ -> do
      numPackets <- parseBin 11
      count numPackets parsePacket
      
parsePacket :: Parser Packet
parsePacket = do
  v <- parseBin 3
  t <- parseBin 3
  payload <- case t of
    0 -> POp Sum <$> parseOperator
    1 -> POp Product <$> parseOperator
    2 -> POp Min <$> parseOperator
    3 -> POp Max <$> parseOperator
    4 -> PLit <$> parseLiteral
    5 -> POp GTo <$> parseOperator
    6 -> POp LTo <$> parseOperator
    7 -> POp EQo <$> parseOperator
    _ -> fail "bad payload id"
  pure (P v payload)
  
main :: IO ()
main = do
  input <- getContents
  let bits = concatMap  (printf "%04b") ((map (fst . head . readHex . (:[])) $ head $ lines input)::[Int])::String
  let res = parse parsePacket "" bits
  case res of
    Left error -> putStrLn $ errorBundlePretty error
    Right result -> putStrLn $ "Part 1: " ++ show (addVersions result) ++ "\nPart 2: " ++ show (eval result)

addVersions :: Packet -> Int
addVersions (P v (PLit _)) = v
addVersions (P v (POp _ ps)) = v + sum (map addVersions ps)
            
eval :: Packet -> Int
eval (P v (PLit num)) = num
eval (P v (POp Sum ps)) = sum $ map eval ps
eval (P v (POp Product ps)) = product $ map eval ps
eval (P v (POp Min ps)) = minimum $ map eval ps
eval (P v (POp Max ps)) = maximum $ map eval ps
eval (P v (POp GTo [l,r])) = if eval l>eval r then 1 else 0 
eval (P v (POp LTo [l,r])) = if eval l<eval r then 1 else 0 
eval (P v (POp EQo [l,r])) = if eval l==eval r then 1 else 0



