

{-# OPTIONS_GHC -Wall #-}

main :: IO()
main = return() --can just test in ghci

type Peg = String
type Move = (Peg, Peg)

-- 3 pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi disks a b c = acTempB ++ abTop ++ cbTempA 
                    where acTempB = hanoi (disks-1) a c b
                          abTop = hanoi 1 a b c
                          cbTempA = hanoi (disks-1) c b a

-- 4 pegs
-- similar as with 3 pegs
hanoi4Pegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Pegs 0 _ _ _ _ = []
hanoi4Pegs 1 a b _ _ = [(a, b)]
hanoi4Pegs 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4Pegs n a b c d = (hanoi4Pegs (n-2) a c d b) ++ (hanoi4Pegs 2 a b d c) ++ (hanoi4Pegs (n-2) c b a d)
