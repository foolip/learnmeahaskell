nonNegative :: (Num a, Ord a) => a -> Bool
nonNegative = (0<=)

forEach :: (a -> a) -> [a] -> [a]
forEach _ [] = []
forEach f (x:xs) = f x : forEach f xs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (f (f x))

applyFourTimes :: (a -> a) -> a -> a
applyFourTimes = applyTwice applyTwice

applySevenTimes :: (a -> a) -> a -> a
applySevenTimes f x = applyFourTimes f (applyThrice f x)

applyTwelveTimes :: (a -> a) -> a -> a
applyTwelveTimes f = applyFourTimes (applyThrice f)

applyTimes :: (Integral t) => t -> (a -> a) -> a -> a
applyTimes 1 f x = f x
applyTimes n f x | n > 1 = applyTimes (n - 1) f (f x)

mul3 :: (Num a) => a -> a
mul3 x = applyTimes 3 (x+) 0
