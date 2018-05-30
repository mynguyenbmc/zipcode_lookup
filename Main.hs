module Main where
import Class
import Place
import Data.Csv
--import qualified Data.ByteString.Lazy as BL
import Prelude as P
import qualified Data.Vector as V
import Data.HashMap.Strict as H
import System.IO

locationFile = "ziplocs.csv"
populationFile = "uszipcodes.csv"

sepBy :: Char -> String -> [String]
sepBy _ [] = []
sepBy d l@(x:xs)
   | (x == d || x == '\r') = sepBy d xs
   | otherwise = px : sepBy d sx
      where (px, sx) = break (== d) l


getPopulated :: [String] -> Maybe Place
getPopulated [] = Nothing
getPopulated l@(x:xs)
  | length l <= 3 = Just $ Place zipcode town state
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

newLocatedPlace :: Float -> Float -> Place -> Place
newLocatedPlace lat long (Place z t s) = LocatedPlace z t s lat long
newLocatedPlace lat long (PopulatedPlace z t s _ _ popu) = PopulatedPlace z t s lat long popu

getLocated :: HashMap String Place -> [String] -> Maybe Place
getLocated _ [] = Nothing
getLocated h l@(x : xs)
  | H.member x h == False = if length(l) <= 3 then Just $ Place zipcode town state
      else Just $ LocatedPlace zipcode town state lat long
  | otherwise = Just $ newLocatedPlace lat long $ extract oldv
  where
    oldv = H.lookup x h
    zipcode = l !! 0
    town = l !! 2
    state = l !! 3
    lat = read $ l !! 5 :: Float
    long = read $ l !! 6 :: Float
    extract :: Maybe Place -> Place
    extract (Just p) = p

finalMap :: HashMap String Place -> [[String]] -> HashMap String Place
finalMap h [] = h
finalMap h (x : xs) = finalMap (H.insert (head x) (extract $ getLocated h x) h) xs
  where
    extract :: Maybe Place -> Place
    extract (Just p) = p

main = do
  population <- readFile populationFile
  let plines = tail $ lines population
  let populationPlaces = getObjects plines populationMap
  location <- readFile locationFile
  let llines = P.map (sepBy ',') $ tail $ lines location
  let headlen = length $ head llines
  --let places = P.map (\x -> H.insert (head x) (extract $ getLocated populationPlaces x) populationPlaces) llines
  let places = finalMap populationPlaces llines
  let exists = H.lookup "501" places
  putStrLn $ show exists
  --putStrLn population
  where
    populationMap :: HashMap String Place
    populationMap = H.empty
    places :: HashMap String Place
    places = H.empty
