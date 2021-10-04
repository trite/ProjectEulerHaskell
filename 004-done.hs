-- >>> [97..103] `zip` map isPalindrome [97..103]
-- [(97,False),(98,False),(99,True),(100,False),(101,True),(102,False),(103,False)]
isPalindrome x =
    str == reverse str
    where
        str = show x


products = [x*y | x <- [100..999], y <- [100..x]]

filtered = filter isPalindrome products

-- >>> maximum filtered
-- 906609
