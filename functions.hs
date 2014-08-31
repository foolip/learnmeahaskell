nonNegative :: (Num a, Ord a) => a -> Bool
nonNegative = (0<=)

forEach :: (a -> a) -> [a] -> [a]
forEach _ [] = []
forEach f (x:xs) = f x : forEach f xs

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

applyThrice :: (a -> a) -> a -> a
applyThrice f = f . f . f

applyFourTimes :: (a -> a) -> a -> a
applyFourTimes = applyTwice applyTwice

applySevenTimes :: (a -> a) -> a -> a
applySevenTimes f = applyFourTimes f . applyThrice f

applyTwelveTimes :: (a -> a) -> a -> a
applyTwelveTimes = applyFourTimes . applyThrice

applyTimes :: (Integral t) => t -> (a -> a) -> a -> a
applyTimes 1 f x = f x
applyTimes n f x | n > 1 = applyTimes (n - 1) f (f x)

mul3 :: (Num a) => a -> a
mul3 x = applyTimes 3 (x+) 0

sum' :: (Num a) => [a] -> a
sum' = foldl (\sum x -> sum + x) 0

max' :: (Ord a) => [a] -> a
max' = foldl1 (\x y -> if x > y then x else y)
