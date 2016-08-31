module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord
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



scan2 :: (a -> a -> Ordering) -> (x -> x) -> (x -> x) -> (x -> x) -> (x -> x) -> (x -> x) -> x -> [a] -> [a] -> x
scan2 _    _   _   _   _   _   acc [] [] = acc
scan2 comp gtf eqf lsf lmf rmf acc (x:xs) [] = scan2 comp gtf eqf lsf lmf rmf (lmf acc) xs []
scan2 comp gtf eqf lsf lmf rmf acc [] (y:ys) = scan2 comp gtf eqf lsf lmf rmf (rmf acc) [] ys
scan2 comp gtf eqf lsf lmf rmf acc (x:xs) (y:ys) = case x `comp` y of
 LT     -> scan2 comp gtf eqf lsf lmf rmf (lsf acc) xs (y:ys)
 GT     -> scan2 comp gtf eqf lsf lmf rmf (gtf acc) (x:xs) ys
 otherwise   -> scan2 comp gtf eqf lsf lmf rmf (eqf acc) xs ys


stripWhile :: (a -> Bool) -> [a] -> (Int, [a])
stripWhile _ [] = (0, [])
stripWhile f (x:xs) = if f x then g $ stripWhile f xs else (0, x:xs)
  where g (x, y) = (x+1, y)

-- |Get a 5-tuple representing the feedback of a guess, which contains
--  (correct, lower rank, same rank, higher rank, same suit) as ints
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback ts gs
    | (length ts) /= (length gs) = error "number of cards in guess and answer does not match"
    | otherwise                  = getFeedback (sort ts) (sort gs)



getNc ts gs = scan2 compare id (+1) id id id 0 ts gs
getNss ts gs = scan2 (comparing getS) id (+1) id id id 0 ts gs
  where getS (Card s _) = s

getRankNums ts gs = (nlr, nsr, nhr)
  where gsMin = head gs
        (nlr, nts) = stripWhile (<gsMin) ts
        eqf (x, y) = (x+1, y)
        lmf (x, y) = (x, y+1)
        (nsr, nhr) = scan2 compare id eqf id lmf id (0, 0) nts gs



-- assume ts gs sorted
getFeedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
getFeedback ts gs = (nc, nlr, nsr, nhr, nss)
  where nc = getNc ts gs
        nss = getNss ts gs
        getR (Card _ r) = r
        (nlr, nsr, nhr) = getRankNums (sort $ map getR ts) (sort $ map getR gs)








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
        where allCombinations = chooseK allCards x
              guess = getInitialGuess x
              others = delete guess allCombinations























satisfyFeedback :: (Int,Int,Int,Int,Int) -> [Card] -> [Card] -> Bool
satisfyFeedback lastFeedback lastGuess newGuess
    = lastFeedback == (getFeedback newGuess lastGuess)

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
chooseGuess guesses = (fst . (minimumBy compFunc)) guesses
    where compFunc x y = (snd x) `compare` (snd y)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (lastGuess, (GameState possible)) feedback
    = (nextPick, GameState nextPossible)
        where nextPossible = filter (satisfyFeedback feedback lastGuess) possible
              nextPick = if length nextPossible > threshold
                         then head nextPossible
                         else (chooseGuess . computeNumAnswers) nextPossible