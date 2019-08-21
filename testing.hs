{-# LANGUAGE FlexibleInstances #-}

module Testing where

import Control.Monad.State
import Control.Monad.Identity

class Monad m => FSMonad m where
  readF :: FilePath -> m String

numCharactersInFile :: FSMonad m => FilePath -> m Int
numCharactersInFile fileName = do
    contents <- readF fileName
    return (length contents)

instance FSMonad IO where
    readF = Prelude.readFile

data MockFS = SingleFile FilePath String

instance FSMonad (State MockFS) where 
    readF pathRequested = do
        (SingleFile pathExisting contents) <- get
        if pathExisting == pathRequested
            then return contents
            else fail "file not found"

testNumCharactersInFile :: Bool
testNumCharactersInFile = evalState
                                (numCharactersInFile "test.txt") 
                                (SingleFile "test.txt" "hello world")
                             == 11

instance FSMonad Identity where
    readF _ = return "mocked"

testWithIdentity = runIdentity (numCharactersInFile "test.txt")
                   == 6
