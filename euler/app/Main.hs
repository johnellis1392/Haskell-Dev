module Main where

import Solutions.S551 (s551'')

main :: IO ()
main = do
  putStrLn $ "Running..."
  let solution = s551'' (10 ^ 16)
  putStrLn $ "Solution: " ++ show solution 


