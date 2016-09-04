--  File     : Proj1.hs
--  Author   : QIAN Kuan (Jack) [686464]
--  Purpose  : Implementation of card guessing game in Project 1,
--             refer to the Project 1 specification for more details

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord
import Card

-- |Representing the game state, which contains a list of all possible guesses
data GameState = GameState [[Card]]
    deriving (Show)

-- |Threshold value for choosing algorithms
--  if number of potential guesses higher than the threshold,
--  the first potential guess will be chosen, otherwise,
--  the more sophisticated algorithm will be used
threshold :: Int
threshold = 1500

-- |Constant of all cards in ascending order
allCards :: [Card]
allCards = [minBound .. maxBound]

-- |Constant of all ranks in ascending order
allRanks :: [Rank]
allRanks = [minBound .. maxBound]

-- |Constant of all suits in ascending order
allSuits :: [Suit]
allSuits = [minBound .. maxBound]

-- |Get the rank of a card
getRank :: Card -> Rank
getRank (Card _ rank) = rank

-- |Get the suit of a card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit




--                       `feedback`

-- |Scan through 2 sorted list sorted in ascending order
--  takes a compare function, two lists a and b,
--  returns (number of equivalent items, items left in a, items left in b)
--  where the last two values are items in one list larger than the highest
--  item in the other
countEq :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> (Int, [a], [a])
countEq compFunc [] [] = (0, [], [])
countEq compFunc ts [] = (0, ts, [])
countEq compFunc [] gs = (0, [], gs)
countEq compFunc (t:ts) (g:gs) = case t `compFunc` g of 
  GT        -> countEq compFunc (t:ts) gs
  LT        -> countEq compFunc ts (g:gs)
  otherwise -> (countEq compFunc ts gs) `combine` 1
    where combine (x1, y1, z1) x2 = (x1+x2, y1, z1)

-- |Given two sorted lists of cards, return the number of correct guesses
countCorrect :: [Card] -> [Card] -> Int
countCorrect ts gs = a where (a, _, _) = countEq compare ts gs

-- |Given two sorted lists of cards,
--  return the number of cards in the same suit
countSameSuit :: [Card] -> [Card] -> Int
countSameSuit ts gs = a where (a, _, _) = countEq (comparing getSuit) ts gs


-- |Same as dropWhile except it also returns the number of items skipped
dropWhileAndCount :: (a -> Bool) -> [a] -> (Int, [a])
dropWhileAndCount f ls = fun f 0 ls
  where fun f n [] = (n, [])
        fun f n (x:xs) = if f x then fun f (n+1) xs else (n, x:xs)

-- |Takes sorted rank of cards in answer and guesses, returns numbers of
--  (lower ranks, correct ranks, higher ranks)
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
    | (length ts) /= (length gs)
        = error "number of cards in guess and answer does not match"
    | otherwise
        = feedbackOfSorted (sort ts) (sort gs)

-- |The `feedback` function assuming cards in answer and feedback
--  sorted in ascending order
feedbackOfSorted :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedbackOfSorted ts gs = (countCorrect ts gs,
                          nlr, nsr, nhr,
                          countSameSuit ts gs)
  where tsRanks = sort $ map getRank ts
        gsRanks = sort $ map getRank gs
        (nlr, nsr, nhr) = getRankInfo tsRanks gsRanks




--                       `initialGuess`

-- |Takes a list and a number k, return all combinations of k elements
--  in the list
chooseK :: (Eq a) => [a] -> Int -> [[a]]
chooseK ls k
    | k <= 0         = error "n must be a positive integer"
    | k > length ls  = error "n must be less than or equal to length of list"
    | k == length ls = [ls]
    | k == 1         = [[x] | x <- ls]
    | otherwise = withFst ++ withoutFst
        where first = head ls
              others = tail ls
              withFst = [first:comb | comb <- chooseK others (k-1)]
              withoutFst = chooseK others k


-- |Adapted from assignment 1 work, takes every nth element in a list
everyNth :: Int -> [t] -> [t]
everyNth n (x:xs)
 | n <= 0     = error "n is not greater than 0"
 | otherwise  = f n [] (x:xs)
    where f n picked lst = if n > length(lst) then picked
                           else f n (picked ++ [lst!!(n-1)]) (drop n lst)

-- |Takes a number n, returns the initial guess of a combination of n cards
getInitialGuess :: Int -> [Card]
getInitialGuess n = [Card (suits!!i) (ranks!!i) | i <- [0..n-1]]
    where f ls n = max ((length ls) `div` (n+1)) 1
          suits = cycle (everyNth (f allSuits n) allSuits)
          ranks = cycle (everyNth (f allRanks n) allRanks)

-- |Takes a number n, returns the initial guess of a combination of n cards
--  and the initial game state
initialGuess :: Int -> ([Card],GameState)
initialGuess x
    | x <= 0     = error "card number must be a positive integer"
    | otherwise  = (guess, GameState others)
        where allComb = map sort (chooseK allCards x)
              guess = if length allComb < threshold
                      then chooseGuess . computeSplitFactors $ allComb
                      else getInitialGuess x
              others = delete guess allComb




--                       `nextGuess`

-- |Takes the last feedback, last guess and potential new guess
--  returns true if the new guess could be the correct one
satisfyFeedback :: (Int,Int,Int,Int,Int) -> [Card] -> [Card] -> Bool
satisfyFeedback lastFeedback lastGuess newGuess
    = lastFeedback == (feedbackOfSorted newGuess lastGuess)

-- |Takes a candidate guess and other possible guesses, return
--  a number showing how well this guess splits the others from a
--  feedback, with smaller number being better
splitFactor :: [Card] -> [[Card]] -> Int
splitFactor guess others = f . (map length) . group . sort $ feedbacks
    where feedbacks = [feedbackOfSorted answer guess | answer <- others]
          f bin = (sum (map (^2) bin)) `div` (sum bin)

-- |Takes a list of all guesses, return a list of tuples containing
--  every guesses and their split factors
computeSplitFactors :: [[Card]] -> [([Card], Int)]
computeSplitFactors guesses
    = [(x, splitFactor x (delete x guesses)) | x <- guesses]

-- |Takes a list of all guesses, return the best guess evaluated
chooseGuess :: [([Card], Int)] -> [Card]
chooseGuess = fst . (minimumBy $ comparing snd)

-- |Takes the last guess, game state, and the last feedback,
--  returns the next guess and game state
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (lastGuess, (GameState possible)) feedback
    = (nextPick, GameState nextPossible)
        where f = satisfyFeedback feedback lastGuess
              nextPossible = filter f possible
              nextPick = if length nextPossible > threshold
                         then head nextPossible
                         else chooseGuess . computeSplitFactors $ nextPossible