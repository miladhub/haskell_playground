module Chap10 where

import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime 
        (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate d) : xs) = d : filterDbDate xs
filterDbDate (_ : xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber n) : xs) = n : filterDbNumber xs
filterDbNumber (_ : xs) = filterDbNumber xs

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' items = foldl f [] items

f :: [UTCTime] -> DatabaseItem -> [UTCTime]
f acc (DbDate d) = d : acc
f acc _ = acc

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' items = foldl g [] items

g :: [Integer] -> DatabaseItem -> [Integer]
g acc (DbNumber n) = n : acc
g acc _ = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent [(DbDate d)] = d
mostRecent items =
    let dates = filterDbDate items
        first = (head dates)
    in foldl max (head dates) dates

sumDb :: [DatabaseItem] -> Integer
sumDb items = sum $ filterDbNumber items

avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral theSum) / (fromIntegral theNum)
    where theSum = sumDb items
          theNum = length (filterDbNumber items)
          
fact :: [Int]
fact = scanl (*) 1 [1..]

factN :: Int -> Int
factN n = fact !! n

stops  = "pbtdkg"
vowels = "aeiou"

svs = [ (s, w, s') | s <- stops, w <- vowels, s' <- stops ]
pvs = [ (s, w, s') | s <- stops, w <- vowels, s' <- stops, s == 'p' ]

nouns = ["house", "tree", "street"]
verbs = ["is", "has", "goes"]

nvn = [ (n,v,n') | n <- nouns, v <- verbs, n' <- nouns ]

seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

seekritFunc' :: (Fractional a) => String -> a
seekritFunc' x =
    (/) ( fromIntegral (sum (map length (words x))) )
        (fromIntegral (length (words x)) )


myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : _) = True
myOr (False : xs) = myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x : xs) = x || myOr xs

myOr'' :: [Bool] -> Bool
myOr'' bs = foldr (||) False bs

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (fmap f)

myElem :: Eq a => a -> [a] -> Bool
myElem e = (foldr (||) False) . (fmap (== e))
--myElem e [] = False
--myElem e (x : xs) = (e == x) || (myElem e xs)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr ((:) . f) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if (f x) then (x : xs) else xs) []

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f = squish . (myMap f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f as =
    foldl g (head as) as
    where g a' a'' = if (f a' a'' == GT) then a' else a''

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f as =
    foldl g (head as) as
    where g a' a'' = if (f a' a'' == LT) then a' else a''


