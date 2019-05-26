module Ch8 where

import Data.List (intersperse)

-- Reviewing types
-- 1) D
-- 2) B
-- 3) D
-- 4) B

-- Reviewing currying
-- 1) "woops mrow woohoo!"
-- 2) "1 mrow haha"
-- 3) "woops mrow 2 mrow haha"
-- 4) "woops mrow blue mrow haha"
-- 5) "pink mrow haha mrow green mrow woops mrow blue"
-- 6) "are mrow Pugs mrow awesome"

-- Reviewing Recursion
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

-- dividedBy 15 2
-- go 15 2 0
--   go (15 - 2) 2 (0 + 1)
--     go 13 2 1
--       go (13 - 2) 2 (1 + 1)
--         go 11 2 2
--           go (11 - 2) 2 (2 + 1)
--             go 9 2 3
--               go (9 - 2) 2 (3 + 1)
--                 go 7 2 4
--                   go (7 - 2) 2 (4 + 1)
--                     go 5 2 5
--                       go (5 - 2) 2 (5 + 1)
--                         go 3 2 6
--                           go (3 - 1) 2 (6 + 1)
--                             go 1 2 7
--                             | 1 < 2 = (7, 1)
recSum :: (Eq a, Num a) => a -> a
recSum n = go n 0
  where
    go n s
      | n == 1 = s + 1
      | otherwise = go (n - 1) (s + n)

recMult :: (Integral a) => a -> a -> a
recMult a b = go a b 0
  where
    go a b s
      | b == 0 = s
      | otherwise = go a (b - 1) (s + a)

-- Fixing divided by
data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

fixedDividedBy :: Integer -> Integer -> DividedResult
fixedDividedBy num denom
  | denom == 0 = DividedByZero
  | num < 0 && denom > 0 = Result $ negate (go (-num) denom 0)
  | num > 0 && denom < 0 = Result $ negate (go num (-denom) 0)
  | num < 0 && denom < 0 = Result $ go (-num) (-denom) 0
  | otherwise = Result (go num denom 0)
  where
    go n d count
      | n < d = count
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 Function
mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

-- Numbers into words
digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | n == 0 = []
  | otherwise = (digits (div n 10)) ++ [mod n 10]
    

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
