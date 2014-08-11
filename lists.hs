numbers = [1..20]

-- doesn't work for empty lists
maxList l = if null (tail l)
            then head l
            else max (head l) (maxList (tail l))

-- not very efficient
revList l = if null l
            then []
            else (revList (tail l)) ++ [head l]

twiceList l = [x*2 | x <- l]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
