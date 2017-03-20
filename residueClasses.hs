import Data.List (nub, elemIndex)
import Data.Ratio
import Data.Maybe
import Math.NumberTheory.Primes.Factorisation

main = print "ready"

gcd' :: (Integral a) => a -> a -> a   
gcd' a 0 = a
gcd' a b = gcd b a `rem` b

residueClassModN :: (Integral a) => a -> a -> [a]
--residueClassModN a b = [a ^ c `rem` b | c <- [1..(totient b)]]
residueClassModN a b = if (isPrime b) 
                       then [ c | c <- [1..(totient' (totient' b))]]
                       else [ c | c <- [1..(totient' b)]]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted


isCompleteResidue :: (Integral a) => [a] -> a -> Bool
isCompleteResidue set modulo = 
  quickSort (set) == [1..(modulo - 1)]

primitiveRoots :: (Integral a) => a -> a -> a -> [a]
primitiveRoots a b n = [ a ^ c `mod` n | c <- [1..b], gcd' c b == 1 ]


primeFactors n = primeFactors' n 2
  where
    primeFactors' 1 _ = []
    primeFactors' n f
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

totient' :: (Integral a) => a -> a
totient' 1 = 1
totient' n = numerator ratio `div` denominator ratio
 where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                 (n % 1) $ nub (primeFactors n)

isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n

reduceModN :: (Integral a) => a -> a -> [a]
reduceModN a n = [a ^ c `mod` n | c <- [1..(n-1)]]



applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x 

indicesHelper a n = [ elemIndex x (reduceModN a n) | x <- [1..(n - 1)]] 

indices' a n = [ z `applyMaybe` \x -> Just (x + 1) | z <- indicesHelper a n] 
