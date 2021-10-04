-- >>> map (10 `hasRemainder`) [2..9]
-- [False,True,True,False,True,True,True,True]
hasRemainder :: Integral a => a -> a -> Bool
hasRemainder x = (/= 0) . mod x
-- hasRemainder x y = x `mod` y /= 0

noRemainder :: Integral a => a -> a -> Bool
noRemainder x = (== 0) . mod x

-- >>> map floorSqrt [x^2 | x <- [1..10]]
-- [1,2,3,4,5,6,7,8,9,10]
floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral



-- >>> filter isPrime [2..50]
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
isPrime' :: Int -> Bool
isPrime' = (map isPrime' [0..] !!)
    where isPrime' 2 = True
          isPrime' 3 = True
          isPrime' x = all (hasRemainder x) [2 .. floorSqrt x]


-- >>> genPrimesTill 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
genPrimesTill :: Int -> [Int]
genPrimesTill limit = [n | n <- [2..limit], isPrime' n]

-- >>> genPrimesTill (floorSqrt target)
-- ProgressCancelledException

result = filter (noRemainder target) (genPrimesTill (floorSqrt target))
-- >>> result
-- ProgressCancelledException



-- >>> floorSqrt target
-- 114

-- >>> isPrime 10000019
-- True

target :: Int
target = 600851475143 
-- target = 13195




-- >>> floorSqrt 13195
-- 114



-- -- >>> all (> 3) [5..10]
-- -- True

-- -- >>> all (> 3) [2..10]
-- -- False

-- -- primeTill limit = takeWhile (< limit) [n | n<-[2..], all ((> 0).rem n) [2..n-1]]

-- -- >>> map ((> 0).rem 7) [2..6]
-- -- [True,True,True,True,True]

-- hasRemainder :: Integral a => a -> a -> Bool
-- hasRemainder x = (/= 0) . mod x
-- -- hasRemainder x y = x `mod` y /= 0

-- -- >>> map (7 `hasRemainder`) [2..6]
-- -- [True,True,True,True,True]
-- -- >>> map (7 `hasRemainder`) [2..6]
-- -- [True,True,True,True,True]


-- floorSqrt :: Int -> Int
-- floorSqrt = floor . sqrt . fromIntegral

-- -- >>> takeWhile (< 10) [n ]

-- -- >>> floorSqrt 10
-- -- >>> [2 .. floorSqrt 10]
-- -- 3
-- -- [2,3]


-- -- >>> map floorSqrt [10,20..500]
-- -- [3,4,5,6,7,7,8,8,9,10,10,10,11,11,12,12,13,13,13,14,14,14,15,15,15,16,16,16,17,17,17,17,18,18,18,18,19,19,19,20,20,20,20,20,21,21,21,21,22,22]



-- -- isPrime x = all (hasRemainder x) [2..x-1]

-- isPrime = (map isPrime' [0..] !!)
--     where isPrime' 2 = True
--           isPrime' 3 = True
--           isPrime' x = all (hasRemainder x) [2 .. floorSqrt x]

-- -- prime 2 = True
-- -- prime 3 = True
-- -- prime x = all (hasRemainder x) [2 .. floorSqrt x]

-- -- >>> filter isPrime [2..20]
-- -- [2,3,5,7,11,13,17,19]

-- -- x is prime if all values from 2 to the floor of the square root of x are also prime



-- -- memoizedFib :: Int -> Integer
-- memoizedFib = (map fib [0 ..] !!)
--    where fib 0 = 0
--          fib 1 = 1
--          fib n = memoizedFib (n-2) + memoizedFib (n-1)
