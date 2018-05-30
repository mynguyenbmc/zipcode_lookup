module Parse where

sepBy :: Char -> String -> [String]
sepBy _ [] = []
sepBy d l@(x:xs)
   | x == d = sepBy d xs
   | otherwise = px : sepBy d sx
      where (px, sx) = break (== d) l
