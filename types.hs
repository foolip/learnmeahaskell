-- duplicating the function name is ugly?
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

half :: Int -> Float
half x = fromIntegral x / 2

-- how to make a positive integer type?
factorial :: Integer -> Integer
factorial n = if n == 1
              then 1
              else n * factorial (n - 1)
