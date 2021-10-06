-- >>> sumOfSquares 10
-- 385
sumOfSquares limit = sum [x^2 | x <- [1..limit]]

-- >>> squareOfSums 10
-- 3025
squareOfSums limit = sum [1..limit] ^ 2


-- >>> sumSquareDiff 10
-- 2640
sumSquareDiff limit = squareOfSums limit - sumOfSquares limit


-- >>> sumSquareDiff 100
-- 25164150
