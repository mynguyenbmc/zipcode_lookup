{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
module Place where

import Prelude
import Data.Kind

data Place = Place { zipCodeP :: String
                    , townP :: String
                    , stateP :: String }
                    deriving (Eq, Show, Read, Ord)

data LocatedPlace = LocatedPlace { zipCodeLP :: String
                                  , townLP :: String
                                  , stateLP :: String
                                  , latLP :: Float
                                  , lonLP :: Float }
                                  deriving (Eq, Show, Read, Ord)

data PopulatedPlace = PopulatedPlace { zipCodePP :: String
                                  , townPP :: String
                                  , statePP :: String
                                  , latPP :: Float
                                  , lonPP :: Float
                                  , populationPP :: Int }
                                  deriving (Eq, Show, Read, Ord)

data GeneralPlace p where
  Nil :: GeneralPlace p
  NewPlace :: p -> GeneralPlace p

getPlace :: GeneralPlace p -> Maybe p
getPlace Nil = Nothing
getPlace (NewPlace p) = Just p
