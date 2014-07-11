{-# LANGUAGE BangPatterns #-}
{-
O(n) at all optimisation
    sum1, sum4, sum5

O(1) at -O1 and above
    sum, sum2, sum6

O(1) at all optimisation
    sum3, sum7
-}

import Data.List
import Data.Monoid


main = print $ mean [1..1000000]


mean xs = sum7 xs `div` length xs
-- print (sum8 [1..10000000] :: Int)


sum1 (x:xs) = x + sum1 xs
sum1 [] = 0

sum2 = f 0
    where f i (x:xs) = f (i+x) xs
          f i [] = i

sum3 = f 0
    where f !i (x:xs) = f (i+x) xs
          f !i [] = i

sum4 (x:xs) = sum4 xs + x
sum4 [] = 0

sum5 = foldr (+) 0
sum6 = foldl (+) 0
sum7 = foldl' (+) 0


sum8 = getSum . mconcat . map Sum
