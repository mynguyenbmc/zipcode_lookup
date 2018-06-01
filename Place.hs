{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
module Place where

import Prelude

data Place where
  Place :: String -> String -> String -> Place
  LocatedPlace :: String -> String -> String -> Float -> Float -> Place
  PopulatedPlace :: String -> String -> String -> Float -> Float -> Int -> Place
  deriving (Eq, Show, Read, Ord)
