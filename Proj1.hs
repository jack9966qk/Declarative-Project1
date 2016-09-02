module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord
import Card

data GameState = GameState [[Card]]
    deriving (Show)

threshold :: Int
threshold = 1000000

allCards :: [Card]
allCards = [minBound .. maxBound]

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

allSuits :: [Suit]
allSuits = [minBound .. maxBound]


getRank :: Card -> Rank
getRank (Card _ rank) = rank

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

countEq :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> (Int, [a], [a])
countEq compFunc [] [] = (0, [], [])
countEq compFunc ts [] = (0, ts, [])
countEq compFunc [] gs = (0, [], gs)
countEq compFunc (t:ts) (g:gs) = case t `compFunc` g of 
  GT        -> countEq compFunc (t:ts) gs
  LT        -> countEq compFunc ts (g:gs)
  otherwise -> (countEq compFunc ts gs) `combine` 1
    where combine (x1, y1, z1) x2 = (x1+x2, y1, z1)

countCorrect :: [Card] -> [Card] -> Int
countCorrect ts gs = a where (a, _, _) = countEq compare ts gs

countSameSuit :: [Card] -> [Card] -> Int
countSameSuit ts gs = a where (a, _, _) = countEq (comparing getSuit) ts gs



dropWhileAndCount :: (a -> Bool) -> [a] -> (Int, [a])
dropWhileAndCount f ls = fun f 0 ls
  where fun f n [] = (n, [])
        fun f n (x:xs) = if f x then fun f (n+1) xs else (n, x:xs)


getRankInfo :: [Rank] -> [Rank] -> (Int, Int, Int)
getRankInfo ts gs = (nlr, nsr, nhr)
  where minR = head gs
        (nlr, newts) = dropWhileAndCount (<minR) ts
        (nsr, tsHigher, _) = countEq compare newts gs
        nhr = length tsHigher



-- |Get a 5-tuple representing the feedback of a guess, which contains
--  (correct, lower rank, same rank, higher rank, same suit) as ints
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback ts gs
    | (length ts) /= (length gs) = error "number of cards in guess and answer does not match"
    | otherwise                  = feedbackOfSorted (sort ts) (sort gs)

feedbackOfSorted :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedbackOfSorted ts gs = (countCorrect ts gs,
                          nlr, nsr, nhr,
                          countSameSuit ts gs)
  where tsRanks = sort $ map getRank ts
        gsRanks = sort $ map getRank gs
        (nlr, nsr, nhr) = getRankInfo tsRanks gsRanks



chooseK :: (Eq a) => [a] -> Int -> [[a]]
chooseK ls k
    | k <= 0         = error "n must be a positive integer"
    | k > length ls  = error "n must be less than or equal to length of list"
    | k == length ls = [ls]
    | k == 1         = [[x] | x <- ls]
    | otherwise      = [first:comb | comb <- chooseK others (k-1)] ++ chooseK others k
        where first = head ls
              others = tail ls

everyNthRec :: Int -> [t] -> [t] -> [t]
everyNthRec n picked lst
 | n > length(lst)  =  picked
 | otherwise        =  everyNthRec n (picked ++ [lst!!(n-1)]) (drop n lst)

everyNth :: Int -> [t] -> [t]
everyNth n (x:xs)
 | n <= 0     = error "n is not greater than 0"
 | otherwise  = everyNthRec n [] (x:xs)

getInitialGuess :: Int -> [Card]
getInitialGuess n = [Card (suits!!i) (ranks!!i) | i <- [0..n-1]]
    where f ls n = max ((length ls) `div` (n+1)) 1
          suits = cycle (everyNth (f allSuits n) allSuits)
          ranks = cycle (everyNth (f allRanks n) allRanks)

initialGuess :: Int -> ([Card],GameState)
initialGuess x
    | x <= 0     = error "card number must be a positive integer"
    | otherwise  = (guess, GameState others)
        where allCombinations = map sort (chooseK allCards x)
              guess = getInitialGuess x
              others = delete guess allCombinations




satisfyFeedback :: (Int,Int,Int,Int,Int) -> [Card] -> [Card] -> Bool
satisfyFeedback lastFeedback lastGuess newGuess
    = lastFeedback == (feedbackOfSorted newGuess lastGuess)

binFeedbacks :: [Card] -> [[Card]] -> [Int]
binFeedbacks guess others = map length grouped
    where grouped = (group . sort) [feedbackOfSorted answer guess | answer <- others]

magicFormula :: [Int] -> Int
magicFormula bin = (sum (map (^2) bin)) `div` (sum bin)

getNumPossibleAnswer :: [Card] -> [[Card]] -> Int
getNumPossibleAnswer g gs = magicFormula (binFeedbacks g gs)

computeNumAnswers :: [[Card]] -> [([Card], Int)]
computeNumAnswers guesses
    = [(x, getNumPossibleAnswer x (delete x guesses)) | x <- guesses]

chooseGuess :: [([Card], Int)] -> [Card]
chooseGuess guesses = (fst . (minimumBy compFunc)) guesses
    where compFunc x y = (snd x) `compare` (snd y)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (lastGuess, (GameState possible)) feedback
    = (nextPick, GameState nextPossible)
        where nextPossible = filter (satisfyFeedback feedback lastGuess) possible
              nextPick = if length nextPossible > threshold
                         then head nextPossible
                         else (chooseGuess . computeNumAnswers) nextPossible