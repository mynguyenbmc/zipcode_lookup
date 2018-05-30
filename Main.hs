module Main where
import Class
import Place
import Data.Csv
--import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.HashMap.Strict as H
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
  where
    zipcode = l !! 0
    town = l !! 1
    state = l !! 2

getObjects :: [String] -> HashMap String Place -> HashMap String Place--[Maybe Place]
getObjects [] h = h
getObjects (x : xs) h = getObjects xs $ H.insert (obj !! 0) (extract $ getPopulated obj) h
  where
    obj = sepBy ',' x
    extract :: Maybe Place -> Place
    extract (Just p) = p


getLocated :: HashMap String Place -> [String] -> Maybe Place
getLocated _ [] = Nothing
getLocated h (x : xs) =
  where 



main = do
  population <- readFile populationFile
  let plines = tail $ lines population
  let populationPlaces = getObjects plines populationMap


  location <- readFile locationFile
  let llines = tail $ lines location

  let exists = H.member "544" populationPlaces
  putStrLn $ show exists
  --putStrLn population
  where
    populationMap :: HashMap String Place
    populationMap = H.empty
    placeMap :: HashMap String Place
    placeMap = H.empty
