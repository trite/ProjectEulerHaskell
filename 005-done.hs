-- >>> map (35 `evenlyDivides`) [3,5,7,9]
-- [False,True,True,False]
evenlyDivides :: Int -> Int -> Bool
evenlyDivides x y = x `mod` y == 0

-- >>> map ([3,5,7] `listEvenlyDivides`) [85,95..125]
-- [False,False,True,False,False]
listEvenlyDivides :: [Int] -> Int -> Bool
listEvenlyDivides ys x = all (x `evenlyDivides`) ys

range' = [2..10]
increment' = 10*9
generator' = [increment',increment'*2..]
result' = take 1 [x | x <- generator', listEvenlyDivides range' x]
-- >>> result'
-- [2520]

range = [2..20]
increment = 20*19*18
generator = [increment,increment*2..]
result = take 1 [x | x <- generator, listEvenlyDivides range x]
-- >>> result
-- [232792560]










{-
>>> map (2520 `div`) [1..10]
[2520,1260,840,630,504,420,360,315,280,252]
>>> map (2520 `div`) [10*9,9*8,8*7,7*6,6*5,5*4,4*3]
[28,35,45,60,84,126,210]

>>> 10*9*7*4
2520
>>> 9*8*7*5
2520
>>> 8*7*9*5 --(9*8*7*5)
2520
>>> 7*6*5*4*3
2520
>>> 10*9*8
720
>>> 2520/720
3.5
-}

-- fDiv :: Int -> Int -> Float
-- x `fDiv` y = fromIntegral x / fromIntegral y

-- ans :: Int
-- ans = 232792560

-- test :: [Int]
-- test = [20*19*18,19*18*17,18*17*16,20*19*18*17*16*15]
{-
>>> map (ans `fDiv`) [1..20]
[2.3279256e8,1.1639628e8,7.759752e7,5.819814e7,4.6558512e7,3.879876e7,3.325608e7,2.909907e7,2.586584e7,2.3279256e7,2.116296e7,1.939938e7,1.790712e7,1.662804e7,1.5519504e7,1.4549535e7,1.369368e7,1.293292e7,1.225224e7,1.1639628e7]
>>> map (ans `fDiv`) test
[34034.0,40040.0,47547.5,8.341666]
-}
