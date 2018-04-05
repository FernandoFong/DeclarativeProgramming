module Monada where

f = do
  x <- [1,2];
  y <- [3,4];
  return (x,y)

testMaybe :: Int -> Maybe Int
testMaybe n = if (mod n  2) == 0 then Just n else Nothing
