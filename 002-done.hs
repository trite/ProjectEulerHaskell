-- fibGen :: Int -> Int -> Int -> Int -> [Int]
-- fibGen limit x y acc
--     | y >= limit = acc
--     | otherwise  = x : fibGen limit y (x+y) acc

fibGen :: (Eq a, Num a, Num p) => a -> p
fibGen 0 = 1
fibGen 1 = 1
fibGen x = fibGen (x-1) + fibGen (x-2)

-- >>> map fibGen [1..10]
-- [1,2,3,5,8,13,21,34,55,89]


fibUntil x = takeWhile (< x) [fibGen y | y <- [1..]]

-- >>> fibUntil 1000000
-- [1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]

-- >>> [x | x <- fibUntil 100, even x]
-- [2,8,34]

-- >>> sum [x | x <- fibUntil 4000000, even x]
-- ProgressCancelledException
-- ^ took too long, needs memoization if not using any other tricks or it'll be slow due to double recursive callbacks in fibGen (fibGen x = fibGen (x-1) + fibGen (x-2))



fiblist :: [Integer]
fiblist = [fibm x | x <- [0..]]

fibm :: Int -> Integer
fibm 0 = 1
fibm 1 = 1
fibm x = fiblist !! (x-1) + fiblist !! (x-2)

-- >>> map fibm [1..10]
-- [1,2,3,5,8,13,21,34,55,89]


fibUntil' x = takeWhile (< x) [fibm y | y <- [1..]]

-- >>> fibUntil' 4000000
-- [1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578]

-- >>> sum [x | x <- fibUntil' 4000000, even x]
-- 4613732