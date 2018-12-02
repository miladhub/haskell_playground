module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1, 2, 3]
y = [4, 5, 6] 
z = [7, 8, 9]

mylookup :: Eq a => a -> [(a, b)] -> Maybe b
mylookup a abs = 
  getFirst $ foldMap First $ fmap (match a) abs
  where
    match a (a', b) | a == a'   = Just b
                    | otherwise = Nothing
