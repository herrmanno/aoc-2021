module Day16 where

import Puzzle ( Puzzle )
import Util (readHex, decimalToBinary, binaryToDecimal')
import Control.Monad.State.Lazy (modify, evalState, get, State)
import Data.Function (on)

part1 :: Puzzle Int
part1 = sumVersions' . head . parsePackages . parseToBinary

part2 :: Puzzle Int
part2 = evalPackage . head . parsePackages . parseToBinary

type Version = Int
type ID = Int

data Packet = Operator Version ID [Packet] | Literal Version ID Int deriving (Show)

sumVersions' :: Packet -> Int
sumVersions' (Operator version _ ps) = version + sum (sumVersions' <$> ps)
sumVersions' (Literal version _ ps) = version

evalPackage :: Packet -> Int
evalPackage (Literal _ _ i) = i
evalPackage (Operator _ 0 ps) = sum (fmap evalPackage ps)
evalPackage (Operator _ 1 ps) = product (fmap evalPackage ps)
evalPackage (Operator _ 2 ps) = minimum (fmap evalPackage ps)
evalPackage (Operator _ 3 ps) = maximum (fmap evalPackage ps)
evalPackage (Operator _ 5 [a,b]) = fromEnum $ ((>) `on` evalPackage) a b
evalPackage (Operator _ 6 [a,b]) = fromEnum $ ((<) `on` evalPackage) a b
evalPackage (Operator _ 7 [a,b]) = fromEnum $ ((==) `on` evalPackage) a b
evalPackage (Operator _ opid _) = error $ "Bad operator id: " <> show opid

parsePackages :: [Int] -> [Packet]
parsePackages = evalState (parsePackage (-1))

parsePackage :: Int -> State [Int] [Packet]
parsePackage n = do
    empty <- null <$> get
    s <- get
    if n == 0 || empty || length s < 11 -- if there are less then 11 bits remaining that cannot be a complete package
        then return []
        else do
            pVersion <- takeNum 3
            pId <- takeNum 3
            p <- case pId of
                4 -> Literal pVersion pId <$> parseLiteralPackage
                _ -> Operator pVersion pId <$> parseOperatorPackage
            ps <- parsePackage (n -1)
            return (p : ps)

parseLiteralPackage :: State [Int] Int
parseLiteralPackage = binaryToDecimal' <$> parseBlock 1
    where parseBlock n = do
            more <- takeNum 1
            bits <- takeBits 4
            if more == 1
                then (bits++) <$> parseBlock (n + 1)
                else return bits

parseOperatorPackage :: State [Int] [Packet]
parseOperatorPackage = do
    lengthType <- takeNum 1
    case lengthType of
        0 -> do
            pLength <- takeNum 15
            childBits <- takeBits pLength
            return $ parsePackages childBits
        _ -> do
            pNum <- takeNum 11
            parsePackage pNum

takeNum n = binaryToDecimal' <$> takeBits n

takeBits n = do
    a <- take n <$> get
    modify (drop n)
    return a

parseToBinary :: String -> [Int]
parseToBinary [] = []
parseToBinary xs = concatMap (decimalToBinary 4 . readHex) xs