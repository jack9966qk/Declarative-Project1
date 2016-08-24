module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Card

data GameState = GameState [[Card]]
    deriving (Show)

-- |Given two lists sorted in ascending order, count the number of elements
--  that are equal, without repitition
countEq :: (Eq t, Ord t) => [t] -> [t] -> Int
countEq [] _ = 0
countEq _ [] = 0
countEq (x:xs) (y:ys)
    | x > y     = countEq (x:xs) ys
    | x < y     = countEq xs (y:ys)
    | otherwise = 1 + countEq xs ys

-- |Given count the number of element that are equal, without repitition
--  in two lists
numEqElem :: (Eq t, Ord t) => [t] -> [t] -> Int
numEqElem a b = countEq (sort a) (sort b)

-- |Get the number of cards in the answer are also in the guess
numCorrect :: [Card] -> [Card] -> Int
numCorrect t g = numEqElem t g

-- |Given a list of cards, get a list of their ranks, preserving order
getRanks :: [Card] -> [Rank]
getRanks cards = [rank | Card _ rank <- cards]

-- |Given a list of cards, get the minimum rank of the cards
getMinRank :: [Card] -> Rank
getMinRank cards = minimum (getRanks cards)

-- |Given a list of cards, get the maximum rank of the cards
getMaxRank :: [Card] -> Rank
getMaxRank cards = maximum (getRanks cards)

-- |Given a list of cards, get a list of their suits, preserving order
getSuits :: [Card] -> [Suit]
getSuits cards = [suit | Card suit _ <- cards]

-- |Get number of cards in the answer have the same rank as
--  a card in the guess
numSameRank :: [Card] -> [Card] -> Int
numSameRank t g = numEqElem (getRanks t) (getRanks g)

-- |Get number of cards in the answer have the same suit as
--  a card in the guess
numSameSuit :: [Card] -> [Card] -> Int
numSameSuit t g = numEqElem (getSuits t) (getSuits g)

-- |Get number of cards cards in the answer have rank
--  lower than the lowest rank in the guess
numLowerRank :: [Card] -> Rank -> Int
numLowerRank cards min = length [42 | rank <- (getRanks cards), rank < min]

-- |Get number of cards cards in the answer have rank
--  higher than the highest rank in the guess
numHigherRank :: [Card] -> Rank -> Int
numHigherRank cards max = length [42 | rank <- (getRanks cards), rank > max]

-- |Get a 5-tuple representing the feedback of a guess, which contains
--  (correct, lower rank, same rank, higher rank, same suit) as ints
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback ts gs
    | (length ts) /= (length gs) = error "number of cards in guess and answer does not match"
    | otherwise                  = (numCorrect ts gs,
                                    numLowerRank ts (getMinRank gs),
                                    numSameRank ts gs,
                                    numHigherRank ts (getMaxRank gs),
                                    numSameSuit ts gs)

chooseN :: (Eq a) => [a] -> Int -> [[a]]
chooseN ls n
    | n <= 0     = error "n must be a positive integer"
    | n == 1     = [[x] | x <- ls]
    | otherwise  = nub [x:others | x <- ls, others <- (chooseN (delete x ls) (n-1))]

initialGuess :: Int -> ([Card],GameState)
initialGuess x
    | x <= 0     = error "card number must be a positive integer"
    | otherwise  = (first, GameState others)
        where all_combinations = chooseN [minBound..maxBound] x
              first = if x == 2 then [Card Club R5, Card Diamond R10]
                      else (chooseGuess . computeNumAnswers) all_combinations
              others = delete first all_combinations

satisfyFeedback :: (Int,Int,Int,Int,Int) -> [Card] -> [Card] -> Bool
satisfyFeedback lastFeedback lastGuess newGuess
    = lastFeedback == (feedback newGuess lastGuess)

binFeedbacks :: [Card] -> [[Card]] -> [Int]
binFeedbacks guess others = map length grouped
    where grouped = (group . sort) [feedback answer guess | answer <- others]

magicFormula :: [Int] -> Int
magicFormula bin = (sum (map (^2) bin)) `div` (sum bin)

getNumPossibleAnswer :: [Card] -> [[Card]] -> Int
getNumPossibleAnswer g gs = magicFormula (binFeedbacks g gs)

computeNumAnswers :: [[Card]] -> [([Card], Int)]
computeNumAnswers guesses
    = [(x, getNumPossibleAnswer x (delete x guesses)) | x <- guesses]

chooseGuess :: [([Card], Int)] -> [Card]
chooseGuess guesses = (fst . head . (sortBy compFunc)) guesses
    where compFunc x y = (snd x) `compare` (snd y)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (lastGuess, (GameState possible)) feedback
    = (nextPick, GameState nextPossible)
        where nextPossible = filter (satisfyFeedback feedback lastGuess) possible
              nextPick = (chooseGuess . computeNumAnswers) nextPossible
              others = delete nextPick nextPossible