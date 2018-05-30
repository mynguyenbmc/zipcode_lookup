module Main where
import Class
import Place
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO

-- sepBy :: String -> String -> [String]

getObject :: [ByteString] -> [Place]
getObject [] = []
getObject x : xs =  : getObject xs
  where obj = sepBy ',' (unpack x)

main = do
  csvData <- readFile "ziplocs.csv"
  let blines = lines csvData
