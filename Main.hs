module Main where
import Class
import Place
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO

sepBy :: Char -> String -> [String]
sepBy _ [] = []
sepBy d l@(x:xs)
   | x == d = sepBy d xs
   | otherwise = px : sepBy d sx
      where (px, sx) = break (== d) l

getObject :: [ByteString] -> [Place]
getObject [] = []
getObject x : xs =  : getObject xs
  where obj = sepBy ',' (unpack x)

main = do
  csvData <- readFile "ziplocs.csv"
  let blines = lines csvData
  
