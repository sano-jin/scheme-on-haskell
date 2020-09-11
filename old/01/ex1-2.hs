module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let a = read $ args !! 0
  let b = read $ args !! 1
  let r = a + b
  putStrLn("Hello, " ++ show r)
