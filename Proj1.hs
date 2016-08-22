module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Card

data GameState = Void

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

initialGuess :: Int -> ([Card],GameState)
initialGuess _ = ([], Void)
-- TODO

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], Void)
-- TODO