import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev

monadic :: [Char] -> ([Char], [Char])
monadic = do
  capped <- cap
  revved <- rev
  return (capped, revved)

monadic'  :: [Char] -> ([Char], [Char])
monadic' = cap >>= (\x -> rev >>= (\y -> return $ (x, y)))