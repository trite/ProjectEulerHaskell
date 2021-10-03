-- fibGen :: Int -> Int -> Int -> Int -> [Int]
-- fibGen limit x y acc
--     | y >= limit = acc
--     | otherwise  = x : fibGen limit y (x+y) acc


fibGen' 1 = 1
fibGen' 2 = 2
fibGen' x = (x-1) + (x-2)

-- >>> fibGen' 10
-- 17

-- >>> map fibGen' [1..10] 
-- [1,2,3,5,7,9,11,13,15,17]


-- >>> 1+1+1+1
-- 4
