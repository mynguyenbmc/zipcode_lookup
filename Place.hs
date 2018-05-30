module Place where

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
  newPlace :: p -> GeneralPlace p
