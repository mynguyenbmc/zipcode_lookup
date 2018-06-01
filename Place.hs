{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
module Place where

import Prelude

data Place where
  Place :: Int -> String -> String -> Place
  LocatedPlace :: Int -> String -> String -> Float -> Float -> Place
  PopulatedPlace :: Int -> String -> String -> Float -> Float -> Int -> Place
  deriving (Eq, Show, Read, Ord)
