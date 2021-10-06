import Data.List (findIndices)
import Data.Char (digitToInt)
{-
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
-}


groupSize :: Int
groupSize = 4

numList = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

-- >>> take 10 indices
-- [13,57,65,79,86,107,124,142,145,154]
indices :: [Int]
indices = findIndices (`elem` "0") numList

-- >>> take 10 (map ranges indices)
-- [(10,16),(54,60),(62,68),(76,82),(83,89),(104,110),(121,127),(139,145),(142,148),(151,157)]
ranges :: Int -> (Int, Int)
ranges x = (x - (groupSize - 1), x + (groupSize - 1))

-- >>> something (10,16) numList 0 []

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ((aMin, aMax):rest) =
    mergeRangesInner [] (aMin, aMax) rest
    where
        mergeRangesInner :: [(Int, Int)] -- ^ accumulator
            -> (Int, Int) -- ^ current accumulating pair
            -> [(Int, Int)] -- ^ rest of the list
            -> [(Int, Int)]
        mergeRangesInner acc head [] = head:acc
        mergeRangesInner acc (accMin, accMax) ((curMin, curMax):rest)
            | accMax > curMin = mergeRangesInner acc (accMin, curMax) rest
            | otherwise       = mergeRangesInner ((accMin, accMax):acc) (curMin, curMax) rest


-- >>> take 1 (drop 13 numList)
-- "0"

takingThings :: [[Char]] -> [Char] -> Int -> [(Int, Int)] -> [[Char]]
-- takingThings acc last rest =
takingThings acc _ _ [] = reverse acc
takingThings acc numList last ((curMin, curMax):rest) =
    takingThings (take (curMin - last) (drop last numList):acc) numList curMax rest

-- >>> takingThings [] numList 0 test
-- ["7316717653","49192251196744265747423553491949349698","27","26239578","9","694788518438586","91129494954","379583319528","111","8747158523","569","3295227","5576689664","52445231617318","7111217223831136222989342","353362766142","444866452387","9","","","","58","24271218839987","922749","9","77665727","36788","54","5125","4752243525","1","395864","2441572215539753697817977846","95514","256932197846862248283972241375","729686524145","48216","9","9524","85412275886668811642717147992444292","3465674813919123162824586178664583591245665294765456828489128831","24","","6263211","42","4165","","245544436298","78799272442","888","6","9191338754","68991","861164","","569831","9357297257163626956188","82524","325","296"]

multList :: Int -> [Char] -> Int
multList = foldl (\ total cur -> total * digitToInt cur)


test3 = digitToInt

-- >>> multList 1 "753"
-- 105
-- >>> digitToInt '5'
-- 5
-- >>> digitToInt (head "5")
-- 5


-- >>> take (10 - 0) numList
-- "7316717653"
test2 = take 10 numList

slice numList start end = take

test = reverse (mergeRanges (map ranges indices))

-- >>> map ranges indices
-- [(10,16),(54,60),(62,68),(76,82),(83,89),(104,110),(121,127),(139,145),(142,148),(151,157),(167,173),(169,175),(178,184),(191,197),(207,213),(227,233),(229,235),(260,266),(262,268),(280,286),(298,304),(303,309),(310,316),(316,322),(319,325),(325,331),(331,337),(339,345),(343,349),(348,354),(368,374),(380,386),(387,393),(391,397),(405,411),(406,412),(408,414),(419,425),(427,433),(437,443),(453,459),(460,466),(464,470),(468,474),(480,486),(514,520),(525,531),(561,567),(564,570),(569,575),(574,580),(592,598),(593,599),(604,610),(609,615),(616,622),(617,623),(618,624),(628,634),(669,675),(739,745),(743,749),(744,750),(752,758),(758,764),(771,777),(775,781),(783,789),(793,799),(795,801),(797,803),(803,809),(807,813),(825,831),(842,848),(851,857),(858,864),(874,880),(875,881),(879,885),(890,896),(895,901),(897,903),(909,915),(912,918),(914,920),(920,926),(921,927),(933,939),(934,940),(935,941),(963,969),(974,980),(975,981),(984,990),(987,993),(996,1002)]
-- >>> test
-- [(10,16),(54,60),(62,68),(76,82),(83,89),(104,110),(121,127),(139,148),(151,157),(167,175),(178,184),(191,197),(207,213),(227,235),(260,268),(280,286),(298,309),(310,316),(316,325),(325,331),(331,337),(339,354),(368,374),(380,386),(387,397),(405,414),(419,425),(427,433),(437,443),(453,459),(460,474),(480,486),(514,520),(525,531),(561,580),(592,599),(604,615),(616,624),(628,634),(669,675),(739,750),(752,758),(758,764),(771,781),(783,789),(793,803),(803,813),(825,831),(842,848),(851,857),(858,864),(874,885),(890,903),(909,920),(920,927),(933,941),(963,969),(974,981),(984,993),(996,1002)]
-- >>> reverse (mergeRanges test)
-- [(10,16),(54,60),(62,68),(76,82),(83,89),(104,110),(121,127),(139,148),(151,157),(167,175),(178,184),(191,197),(207,213),(227,235),(260,268),(280,286),(298,309),(310,316),(316,325),(325,331),(331,337),(339,354),(368,374),(380,386),(387,397),(405,414),(419,425),(427,433),(437,443),(453,459),(460,474),(480,486),(514,520),(525,531),(561,580),(592,599),(604,615),(616,624),(628,634),(669,675),(739,750),(752,758),(758,764),(771,781),(783,789),(793,803),(803,813),(825,831),(842,848),(851,857),(858,864),(874,885),(890,903),(909,920),(920,927),(933,941),(963,969),(974,981),(984,993),(996,1002)]

{-
          mergeRangesInner acc (accMin, accMax) ((curMin, curMax):rest)
            | accMax > curMin = mergeRangesInner acc (accMin, curMax) rest
            | otherwise       = mergeRangesInner ((accMin, accMax):acc) (curMin, curMax) rest

[] (10, 16) [(54,60),...]



-}

-- mergeRangesInner :: [(Int, Int)] -- ^ accumulator
--   -> (Int, Int) -- ^ current accumulating pair
--   -> [(Int, Int)] -- ^ rest of the list
--   -> [(Int, Int)]
-- mergeRangesInner _ _ [] = []
-- mergeRangesInner acc (accMin, accMax) ((curMin, curMax):rest)
--     | accMax > curMin = mergeRangesInner acc (accMin, curMax) rest
--     | otherwise       = mergeRangesInner ((accMin, accMax):acc) (curMin, curMax) rest
