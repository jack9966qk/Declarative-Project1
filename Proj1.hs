module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord
import Data.Map.Strict
import Data.Heap
import Card

data GameState = GameState [[Card]]
    deriving (Show)

threshold :: Int
threshold = 100000

allCards :: [Card]
allCards = [minBound .. maxBound]

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

allSuits :: [Suit]
allSuits = [minBound .. maxBound]

-- |Given two lists sorted in ascending order, count the number of elements
--  that are equal, without repitition
countEq :: (Eq t, Ord t) => (t -> t -> Ordering) -> [t] -> [t] -> Int
countEq compFunc [] _ = 0
countEq compFunc _ [] = 0
countEq compFunc (x:xs) (y:ys) = case compFunc x y of
    GT        -> countEq compFunc (x:xs) ys
    LT        -> countEq compFunc xs (y:ys)
    otherwise -> 1 + countEq compFunc xs ys

-- |Get the number of cards in the answer are also in the guess
numCorrect :: [Card] -> [Card] -> Int
numCorrect t g = countEq (compare) t g


-- |Given a list of cards, get the minimum rank of the cards
getMinRank :: [Card] -> Rank
getMinRank cards = minimum (map getRank cards)

-- |Given a list of cards, get the maximum rank of the cards
getMaxRank :: [Card] -> Rank
getMaxRank cards = maximum (map getRank cards)

getRank :: Card -> Rank
getRank (Card _ rank) = rank

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- |Get number of cards in the answer have the same rank as
--  a card in the guess
numSameRank :: [Card] -> [Card] -> Int
numSameRank t g = countEq (comparing getRank) t g

-- |Get number of cards in the answer have the same suit as
--  a card in the guess
numSameSuit :: [Card] -> [Card] -> Int
numSameSuit t g = countEq (comparing getSuit) t g

-- |Get number of cards cards in the answer have rank
--  lower than the lowest rank in the guess
numLowerRank :: [Card] -> Rank -> Int
numLowerRank cards min = length (filter (\c -> (getRank c) < min) cards)

-- |Get number of cards cards in the answer have rank
--  higher than the highest rank in the guess
numHigherRank :: [Card] -> Rank -> Int
numHigherRank cards max = length (filter (\c -> (getRank c) > max) cards)

-- |Get a 5-tuple representing the feedback of a guess, which contains
--  (correct, lower rank, same rank, higher rank, same suit) as ints
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback ts gs
    | (length ts) /= (length gs) = error "number of cards in guess and answer does not match"
    | otherwise                  = feedbackSorted tsSorted gsSorted
      where tsSorted = sort ts
            gsSorted = sort gs


feedbackSorted :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedbackSorted ts gs = (numCorrect ts gs,
                        numLowerRank tsRankSorted minR,
                        numSameRank tsRankSorted gsRankSorted,
                        numHigherRank tsRankSorted maxR,
                        numSameSuit ts gs)
  where minR = getRank $ head gs
        maxR = getRank $ last gs
        tsRankSorted = sortBy (comparing getRank) ts
        gsRankSorted = sortBy (comparing getRank) gs






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
    = lastFeedback == (feedbackSorted newGuess lastGuess)

binFeedbacks :: [Card] -> [[Card]] -> [Int]
binFeedbacks guess others = map length grouped
    where grouped = (group . sort) [feedbackSorted answer guess | answer <- others]

magicFormula :: [Int] -> Int
magicFormula bin = (sum (map (^2) bin)) `div` (sum bin)

computeNumAnswers :: [[Card]] -> [([Card], Int)]
computeNumAnswers guesses
    = [(x, f x guesses) | x <- guesses]
      where f x guesses = magicFormula (binFeedbacks x (delete x guesses))

chooseGuess :: [([Card], Int)] -> [Card]
chooseGuess guesses = (fst . (minimumBy compFunc)) guesses
    where compFunc x y = (snd x) `compare` (snd y)

emptyHeap :: (HeapT Int [Card])
emptyHeap = empty

computeMagicNum :: [Card] -> [Cards] -> Int
computeMagicNum guess answers
  = 

chooseGuess :: [[Card]] -> [Card]
chooseGuess candidates = best
  where Just best = viewHead foldl f empty candidates
        f heap nextGuess = insert (split $ g nextGuess) heap
        g nextGuess = computeMagicNum nextGuess (delete nextGuess candidates)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (lastGuess, (GameState possible)) feedback
    = (nextPick, GameState nextPossible)
        where nextPossible = filter (satisfyFeedback feedback lastGuess) possible
              nextPick = if length nextPossible > threshold
                         then head nextPossible
                         else (chooseGuess . computeNumAnswers) nextPossible