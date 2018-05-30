module Main where
import Class
import Place
import Data.Csv
--import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO

locationFile = "ziplocs.csv"
populationFile = "uszipcodes.csv"

sepBy :: Char -> String -> [String]
sepBy _ [] = []
sepBy d l@(x:xs)
   | x == d = sepBy d xs
   | otherwise = px : sepBy d sx
      where (px, sx) = break (== d) l


getPopulated :: [String] -> Maybe Place
getPopulated [] = Nothing
getPopulated l@(x:xs)
  | length(l) <= 3 = Just $ Place zipcode town state
  | otherwise = Just $ PopulatedPlace zipcode town state (-1.0) (-1.0) (read $ l !! 3 :: Int)
  where zipcode = l !! 0
        town = l !! 1
        state = l !! 2


getObjects :: [String] -> [Maybe Place]
getObjects [] = []
getObjects (x : xs) = getPopulated obj : getObjects xs
  where obj = sepBy ',' x


main = do
  population <- readFile populationFile
  let plines = tail $ lines population
  let populationLines = getObjects plines
  putStrLn population
