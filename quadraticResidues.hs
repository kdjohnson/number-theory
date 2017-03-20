import Data.List (nub)

main = print "ready"

gcd' :: (Integral a) => a -> a -> a   
gcd' a 0 = a
gcd' a b = gcd b a `rem` b

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

quadraticRoots :: (Integral a) => a -> [a]
quadraticRoots m = quickSort (nub [ x ^ 2 `mod` m | x <- [1..(m - 1)]]) 

nonQuadraticResidues :: (Integral a) => a -> [a] -> [a]
nonQuadraticResidues m xs = [ x | x <- [1..(m-1)], x `notElem` xs] 
