
{-# OPTIONS_GHC -Wall #-}

main :: IO()
main = do
    print (validate 4012888888881881)
    print (validate 4012888888881882)
     

-- Checks if the credit is valid.
validate :: Integer -> Bool
validate n = (checkSum n) `mod` 10 == 0

checkSum :: Integer -> Integer
checkSum = sumListDigits . doubleEveryOther . intToList  -- (.) makes composition functions


-- Computes sum of all the digits in all their integers.
-- recursivly take head elem x of list and sum with remainig tail elems
sumListDigits :: [Integer] -> Integer
sumListDigits [] = 0
sumListDigits (x:xs) = ((sum . intToList) x) + (sumListDigits xs)

-- recursivly append digits to list starting from LSB   
intToList :: Integer -> [Integer]
intToList n
    | n <= 0     = []
    | otherwise = intToList (lsbRemoved) ++ [lsb]
    where lsbRemoved = n `div` 10
          lsb = n `mod` 10


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleEveryOtherRight

-- Doubles every second elem from the right.
doubleEveryOtherRight :: [Integer] -> [Integer]
doubleEveryOtherRight = reverse . doubleEveryOtherLeft . reverse 

-- Double every second elem from the left.
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft (x:[]) = [x]
doubleEveryOtherLeft (x:y:zs) = x : 2*y : doubleEveryOtherLeft zs


-- validator does not use this, but its a part of the exercises
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . intToList 

