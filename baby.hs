import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

half :: Int -> Writer String Int
half x = do
        tell ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)

greeter :: Reader String String
greeter = do
    name <- ask
    return ("hello, " ++ name ++ "!")

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2
