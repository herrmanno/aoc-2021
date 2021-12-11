module Main where

import System.Environment (getArgs)
import Puzzles (runPuzzle)

main :: IO ()
main = do
    args <- map read <$> getArgs
    input <- getContents
    case args of
        [d] -> runDay d Both input
        [d,1] -> runDay d First input
        [d,2] -> runDay d Second input
        _ -> putStrLn "Usage: AoC2021 <day> [part]"

data Parts = Both | First | Second

runDay :: Int -> Parts -> String -> IO ()
runDay d parts input = case (parts, runPuzzle d input) of
    (_,      Nothing)       -> print $ show d <> " is not a valid day"
    (First,  Just (r,_))    -> putStrLn r
    (Second, Just (_,r))    -> putStrLn r
    (Both,   Just (r1,r2))  -> putStrLn r1 >> putStrLn r2