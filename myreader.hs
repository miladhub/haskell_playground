import Control.Monad.Reader

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"

f1 = return $ Just 42 :: Reader String (Maybe Int)
f2 = return $ Just 43 :: Reader String (Maybe Int)

res = do
  a1 <- f1
  a2 <- f2
  return $ do
    aa1 <- a1
    aa2 <- a2
    return (aa1 + aa2)

