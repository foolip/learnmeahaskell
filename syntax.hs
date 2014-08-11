isEven :: Int -> Bool
isEven 0 = True
isEven 1 = False
isEven x
  | x > 0 = isEven (x - 2)
  | x < 0 = isEven (x + 2)

-- how to make a positive integer type?
factorial :: Integer -> Integer
factorial 1 = 1
factorial n | n > 1 = n * factorial (n - 1)

-- still not very efficient
revList :: [a] -> [a]
revList [] = []
revList (x:xs) = revList xs ++ [x]

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [w / h ^ 2 | (w, h) <- xs]

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- for some reason this works
factorial' :: Integer -> Integer
factorial' n =
  case n of 1 -> 1
            n -> n * factorial (n - 1)
