module Ch9 where

-- Exercises
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = []
eftBool True False = [True, False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd GT GT = [GT]

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | otherwise = [a] ++ eftInt (a + 1) b

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a > b = []
  | otherwise = [a] ++ eftChar (succ a) b

spaceSplit :: String -> [String]
spaceSplit "" = []
spaceSplit (' ':s) = spaceSplit s
spaceSplit s = w : spaceSplit t
  where
    w = takeWhile (/= ' ') s
    t = takeWhile (/= ' ') s

firstSens = "Tyger Tyger, burning bright\n"
secondSend = "In the forests of the night\n"
thirdSens = "What immortal hand or eye\n"
fourthSens = "Could fame thy fearful symmetry?"
sentences = firstSens ++ secondSend ++ thirdSens ++ fourthSens

myLines :: String -> [String]
myLines "" = []
myLines ('\n':s) = myLines s
myLines s = w : myLines t
  where
    w = takeWhile (/= '\n') s
    t = takeWhile (/= '\n') s

wordsGeneral :: Char -> String -> [String]
wordsGeneral _ "" = []
wordsGeneral _ (c:s) = wordsGeneral c s
wordsGeneral c s = w : wordsGeneral c t
  where
    w = takeWhile (/= c) s
    t = takeWhile (/= c) s