module CIO where

import Types

printError :: C4Error -> IO ()
printError = undefined

type CIO a = ExceptT C4Error IO a

runCIO :: CIO a -> IO ()
runCIO m = do
  res <- (runExceptT m)
  case res of
    Left e  -> do
      printError e
      exitFailure
    Right x -> return ()
