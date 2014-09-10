module Prime (
  isPrime
) where

isPrime :: Integral a => a -> Bool
isPrime n = not $ any (\divisor -> n `mod` divisor == 0) divisors
  where divisors = 2 :[3,5..limit]
        limit = 1 + (floor . sqrt . fromIntegral) n
