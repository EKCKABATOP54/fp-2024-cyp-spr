module Rotate where

rotate :: Int -> [a] -> [a]
rotate n xs
  | null xs = []
  | otherwise = drop n' xs ++ take n' xs
  where n' = n `mod` length xs
{-

-}