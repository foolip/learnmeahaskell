-- sloooow
fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:those, these)
  where (these, those) = split xs

weave :: (Ord a) => [a] -> [a] -> [a]
weave x [] = x
weave [] x = x
weave (x:xs) (y:ys) =
  if x < y
  then x : weave xs (y:ys)
  else y : weave (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort x =
  let (y, z) = split x
  in weave (mergesort y) (mergesort z)
