module Main where
import Place
import Prelude as P
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

getPopulated :: [String] -> Place
getPopulated (z:t:s:p:_) = PopulatedPlace (read z :: Int) t s (-1.0) (-1.0) (read p :: Int)
getPopulated [z,t,s] = Place (read z :: Int) t s
getPopulated _ = error "invalid input"

newLocatedPlace :: Float -> Float -> Place -> Place
newLocatedPlace lat lon (Place z t s) = LocatedPlace z t s lat lon
newLocatedPlace lat lon (PopulatedPlace z t s _ _ p) = PopulatedPlace z t s lat lon p
newLocatedPlace _ _ _ = error "invalid input"

getLocated :: HashMap Int Place -> [String] -> HashMap Int Place
getLocated h (z:zc:t:s:lt:lat:lon:_) = H.adjust (newLocatedPlace (read lat :: Float) (read lon :: Float)) (read z) h
getLocated h _ = h

main = do
  population <- readFile populationFile
  let plines = P.map (sepBy ',') $ tail $ lines population
  let pMap = foldl (\m l@(x:xs) -> H.insert (read x) (getPopulated l) m ) H.empty plines
  location <- readFile locationFile
  let llines = P.map (sepBy ',') $ tail $ lines $ P.filter (/='\"') location
  let finalMap = foldl (\m lst -> getLocated m lst) pMap llines
  putStrLn "Enter zipcode: "
  ans <- getLine
  let zip = read ans :: Int
  case H.lookup zip finalMap of
    Nothing -> putStrLn "Zipcode doesn't exist"
    Just p -> print p
