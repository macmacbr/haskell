module Hello 
  ( 
    sayHello
  , concatUserInput
  , twoo
  )
  where

sayHello :: String -> IO ()
sayHello name = do
  putStrLn ("hello world" ++ name ++ "!")

concatUserInput = do
  x1 <- getLine
  x2 <- getLine
  return (x1 ++ x2)

twoo :: IO Bool
twoo = do
    c <- getChar
    c' <- getChar
    return (c == c')
