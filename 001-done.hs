limit = 1000

isMod3or5 :: Integral a => a -> Bool
isMod3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0

result = [x | x <- [1..limit-1], isMod3or5 x]

mod3or5 :: Integral a => a -> a
mod3or5 x
    | isMod3or5 x = x
    | otherwise   = 0

recursiveVersion :: Int -> Int -> Int
recursiveVersion 0 _ = 0
recursiveVersion x total = mod3or5 x + recursiveVersion (x-1) total

result2 = recursiveVersion (limit-1) 0

-- >>> take 10 result
-- [3,5,6,9,10,12,15,18,20,21]

-- >>> sum result
-- 233168

-- >>> result2
-- 233168
