import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as Set

import qualified Prime

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

splitOnSpace :: String -> [String]
splitOnSpace str = filter (not . isSpace . head) (groupBy ((==) `on` isSpace) str)

-- names look like: Peter Singer
looksLikeName :: String -> Bool
looksLikeName s = length parts >= 2 && all isCapitalized parts
  where isCapitalized s = isUpper (head s) && all isLower (tail s)
        parts = splitOnSpace s

rot13 :: String -> String
rot13 = map (chr . shift . ord)
  where shift c
          | (c >= ord 'A' && c <= ord 'Z') = (((c - ord 'A') + 13) `mod` 26) + ord 'A'
          | (c >= ord 'a' && c <= ord 'z') = (((c - ord 'a') + 13) `mod` 26) + ord 'a'
          | otherwise = c

primes :: Integral a => [a]
primes = 2 : filter Prime.isPrime [3,5..]
