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
  
main :: IO ()
main = do
  input <- getContents


