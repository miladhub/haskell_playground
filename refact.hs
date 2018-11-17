module Refact where

-- Thanks for the great example, Alex
data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- There's a decoder function that makes -- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the -- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer", -- that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)
