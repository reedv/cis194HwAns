import Data.List


main :: IO()
main = return()

---------- Ex 1: wholemeal programming
fun1 :: [Integer] -> Integer
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs
-- for all elems. in the list that are even,
--    do something to each one, (x-2), and accumulate (multiply) them all together

fun1' :: [Integer] -> Integer
fun1' = product 
        . map (subtract 2)
        . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
-- for some num, continually reduce w/ arithmetic depending on even-ness
--    stop when num is reduced to 1
--    accumulate (sum) all of the even reductions

fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/= stopValue)
        . iterate selectionFunct
            where stopValue = 1
                  selectionFunct n = if even n
                                     then (n `div` 2)
                                     else (3*n + 1)


---------- Ex 2: folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- TODO


---------- Ex 3: more folds
-- foldr, applied to a binary operator, a starting value
--    (typically the right-identity of the operator), and a list,
--    reduces the list using the binary operator, from right to left:

-- Returns True if and only if there are an odd number of True values contained in the input list.
--    It does not matter how many False values the input list contains.
xor :: [Bool] -> Bool
xor = foldr (\acc y -> not (acc /= y)) False
-- how this works can be made a bit clearer by writing out how this works on a seq. T T F T

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\acc y -> (f acc):y) []
-- \a -> f a denotes a lambda function that takes a returns f a

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl binOp startVal = foldr (flip binOp) startVal . reverse
-- flip f takes its (first) two arguments in the reverse order of f.


---------- Ex 4: finding primes
-- Start with a list of the integers from 1 to n. From this list, REMOVE all numbers of the form i + j + 2ij where:
--    i,j in N, 1 <= i <= j
--    i + j + 2ij <= n
--    The remaining numbers are doubled then incremented by one, giving a list of the odd prime numbers
--    (i.e., all primes except 2) below 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ removeRemovables
    where removeRemovables = [1..n] \\ removablesList
          removablesList = map makeRemovable
                           . filter (isRemovable)
                           $ cartesianProd [1..n] [1..n]
                                where cartesianProd :: [a] -> [b] -> [(a, b)]
                                      cartesianProd xs ys = [(x,y) | x <- xs, y <- ys]
                                      isRemovable :: (Integer, Integer) -> Bool
                                      isRemovable (i, j) = i + j + 2*i*j <= n
                                      makeRemovable :: (Integer, Integer) -> Integer
                                      makeRemovable (i, j) = i + j + 2*i*j

-- (base Data.List) The \\ function is list difference ((non-associative). In the result of xs \\ ys, the first occurrence
--    of each element of ys in turn (if any) has been removed from xs.
-- filter, applied to a predicate and a list, returns the list of those elements that satisfy the predicate
-- map f xs is the list obtained by applying f to each element of xs
