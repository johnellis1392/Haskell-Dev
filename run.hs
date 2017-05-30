module Main where

import System.Environment (getArgs)
import System.Process (createProcess, proc)
import System.Directory (listDirectory, getCurrentDirectory)
import Data.String.Utils (endswith, startswith, split, join)

import qualified Utils.Consts as Consts


-- runTests appName = do
--   putStrLn $ "Running tests for test '" ++ show appName ++ "'..."
-- 
-- 
-- compile appName = 
--   let targetFile = "./" Consts.mainDir ++ "/" ++ appName ++ "/" Consts.main ++ ".hs"
--       command = "ghc -iUtils " ++ targetFile
--     in createProcess (proc command [])
-- 
-- run appName = 
--   let targetFile = "./" Consts.mainDir ++ "/" ++ appName ++ "/" Consts.main ++ ".hs"
--       command = targetFile ++ "/" ++ Consts.main
--     in createProcess (proc command [])
--   
-- 
-- runProgram appName = do
--   putStrLn $ "Running program '" ++ show appName ++ "'..."
--   compile appName >> run appName

getSolutionDir :: String -> IO [String]
getSolutionDir solution = listDirectory Consts.solutions >>= getSolution >>= listDirectory
  where
  getSolution :: [String] -> IO (Maybe String)
  getSolution [dirs] = return $ case filter ((==) solution) of
    [dir]   -> Just dir
    _       -> Nothing



printErr = do
  putStrLn $ "Usage: ./run [test] [solution_name] [args...]"
  

main = do
  args <- getArgs
  putStr $ "Args:"
  putStrLn $ show args

  -- case args of
  --   [] -> printErr
  --   ("test":_args) -> runTests _args
  --   [arg1] -> runProgram arg1
  --   _ -> printErr



