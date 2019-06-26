exercise1 = do
  print "Exercise 1"
  print $ toDigits 1234 == [1, 2, 3, 4]
  print $ toDigitsRev 1234 == [4, 3, 2, 1]
  print $ toDigits 0 == []
  print $ toDigits (-17) == []

toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

exercise2 = do
  print "Exercise 2"
  print $ doubleEveryOther [8, 7, 6 ,5] == [16, 7, 12, 5]
  print $ doubleEveryOther [1, 2, 3] == [1, 4, 3]

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft x = case x of
  [] -> []
  [x] -> [x]
  [x, y] -> [x, 2 * y]
  [x, y, z] -> [x, 2 * y, z]
  (x:y:zs) -> [x, 2* y] ++ doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

exercise3 = do
  print "Exercise 3"
  print $ sumDigits [16, 7, 12, 5] == 22

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

exercise4 = do
  print "Exercise 4"
  print $ validate 4012888888881881 == True
  print $ validate 4012888888881882 == False

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

exercise5 = print $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String
type Move = (Peg, Peg)
-- Algorithm:
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c = case x of
  1 -> [(a, b)]
  n -> hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
