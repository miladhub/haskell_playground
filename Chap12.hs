module Chap12 where

notThe :: String -> Maybe String
notThe s = if (s == "the") then Just s else Nothing

-- >>> replaceThe "the cow loves us" -- "a cow loves us"
replaceThe :: String -> String
replaceThe s =
    let tokens = splitStr ' ' s
        replaced = fmap (\t -> if (t == "the") then "a" else t) tokens
    in foldl (\x y -> x ++ " " ++ y) (head replaced) (tail replaced) 

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countInList $ splitStr ' ' s

countInList :: [String] -> Integer
countInList [_] = 0
countInList ("the" : next) =
    if (beginsWithVowel $ head next) 
    then (1 + countInList next) 
    else countInList next
countInList _ = 0

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

beginsWithVowel :: String -> Bool
beginsWithVowel "" = False
beginsWithVowel (x : _) = isVowel x

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

isConsonant :: Char -> Bool
isConsonant x = not (isVowel x)

splitStr :: Char -> String -> [String]
splitStr _ "" = []
splitStr c s =
    let frst = takeWhile (/= c) s
        tokens = splitAt (length frst) s
        rest = snd $ splitAt 1 (snd tokens)
    in  frst : splitStr c rest

newtype Word' =
    Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x =
    if ((length $ filter isVowel x) > (length $ filter isConsonant x))
    then Nothing
    else Just $ Word' x

-- As natural as any
-- competitive bodybuilder

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero)) -- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i >= 0     = Just $ posToNat i
    | otherwise = Nothing

posToNat :: Integer -> Nat
posToNat p
    | p == 0    = Zero
    | p > 0     = Succ $ posToNat $ p - 1

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ _ = b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x) : rest) = x : (catMaybes rest)
catMaybes (Nothing : rest) = catMaybes rest

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = undefined

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (Left a : r) = a : lefts' r
lefts' (_ : r) = lefts' r

lefts'' :: [Either a b] -> [a]
lefts'' es =
    foldr f [] es
    where
        f (Left a) l = a : l
        f _ l = l

rights' :: [Either a b] -> [b]
rights' es =
    foldr f [] es
    where
        f (Right b) l = b : l
        f _ l = l

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es =
    foldr f ([], []) es
    where
        f (Left a) (a', b') = (a : a', b')
        f (Right b) (a', b') = (a', b : b')

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' = undefined
