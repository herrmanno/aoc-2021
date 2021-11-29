module Main where

import System.Environment (getArgs)
import Puzzle (runPuzzle)
import Puzzles (getPuzzle)

main :: IO ()
main = do
    args <- map read <$> getArgs
    case args of
        [d] -> runDay d (\(a,b) -> [a,b])
        [d,1] -> runDay d ((:[]) . fst)
        [d,2] -> runDay d ((:[]) . snd)
        _ -> putStrLn "Usage: AoC2021 <day> [part]"

-- |Executes the puzzle(s) for a given day
runDay :: Int -> (forall a. (a,a) -> [a]) -> IO ()
runDay i select = case getPuzzle i of
    (Just p) -> do
        input <- getContents
        mapM_ (`runPuzzle` input) (select p)
    Nothing -> putStrLn $ "Invalid puzzle 'day " <> show i <> "'"
