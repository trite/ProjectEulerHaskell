-- >>> takeWhile (< 775146) [n | n<-[2..], all ((> 0).rem n) [2..n-1]]
-- ProgressCancelledException


-- >>> all (> 3) [5..10]
-- True

-- >>> all (> 3) [2..10]
-- False

-- primeTill limit = takeWhile (< limit) [n | n<-[2..], all ((> 0).rem n) [2..n-1]]

-- >>> map ((> 0).rem 7) [2..6]
-- [True,True,True,True,True]

hasRemainder :: Integral a => a -> a -> Bool
hasRemainder x = (/= 0) . mod x
-- hasRemainder x y = x `mod` y /= 0

-- >>> map (7 `hasRemainder`) [2..6]
-- [True,True,True,True,True]
-- >>> map (7 `hasRemainder`) [2..6]
-- [True,True,True,True,True]


floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

-- >>> takeWhile (< 10) [n ]

-- >>> floorSqrt 10
-- >>> [2 .. floorSqrt 10]
-- 3
-- [2,3]


-- >>> map floorSqrt [10,20..500]
-- [3,4,5,6,7,7,8,8,9,10,10,10,11,11,12,12,13,13,13,14,14,14,15,15,15,16,16,16,17,17,17,17,18,18,18,18,19,19,19,20,20,20,20,20,21,21,21,21,22,22]



-- isPrime x = all (hasRemainder x) [2..x-1]

-- isPrime x = (map prime [0..] !!)
--     where prime 2 = True
--           prime 3 = True
--           prime x = all isPrime [2 .. floorSqrt x]

prime 2 = True
prime 3 = True
prime x = all prime [2 .. floorSqrt x]

-- >>> map prime [2..20]
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]

-- x is prime if all values from 2 to the floor of the square root of x are also prime




-- memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoizedFib (n-2) + memoizedFib (n-1)



-- >>> takeWhile (< 1000000) (2 : [n | n<-[3,5..], all ((> 0).rem n) [3,5..floor.sqrt.fromIntegral$n]])


{-
>>> hasRemainder 6 5
True

-}
