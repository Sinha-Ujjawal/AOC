{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import Data.List (group)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

data SpringStatus = Operational | Damaged deriving (Show, Eq)

type Row = ([Maybe SpringStatus], [Int])
type Input = [Row]

parseSpringStatus :: Char -> Maybe (Maybe SpringStatus)
parseSpringStatus '.' = Just $ Just Operational
parseSpringStatus '#' = Just $ Just Damaged
parseSpringStatus '?' = Just Nothing
parseSpringStatus _   = Nothing

parseNumbers :: T.Text -> Maybe [Int]
parseNumbers = traverse (readMaybe . T.unpack) . T.splitOn ","

parseRow :: T.Text -> Maybe Row
parseRow (T.words -> [spintStatusesStr, valuesStr]) = do
    spintStatuses <- traverse parseSpringStatus $ T.unpack spintStatusesStr
    values        <- parseNumbers valuesStr
    return (spintStatuses, values)
parseRow _ = Nothing

parseInputFromString :: T.Text -> Maybe Input
parseInputFromString = traverse parseRow . T.lines

parseInputFromFile :: String -> IO (Maybe Input)
parseInputFromFile = fmap parseInputFromString . T.readFile

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = [[]]
cartesianProduct (xs:xss) = [x:xs' | x <- xs, xs' <- cartesianProduct xss]

enumPossibilities :: [Maybe SpringStatus] -> [[SpringStatus]]
enumPossibilities = cartesianProduct . fmap go
  where
    go (Just x) = [x]
    go Nothing  = [Operational, Damaged]

satisfy :: [Int] -> [SpringStatus] -> Bool
satisfy values springStatuses = values == (fmap length . filter ((== Damaged) . head) $ group springStatuses)

solve :: Input -> [[[SpringStatus]]]
solve = fmap (\(springStatuses, values) -> filter (satisfy values) $ enumPossibilities springStatuses)

solvePart1ForInput :: Input -> Int
solvePart1ForInput = sum . fmap length . solve

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    inputMaybe <- parseInputFromFile filePath
    case inputMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just input -> return $ solvePart1ForInput input

main :: IO ()
main = do
    filePath   <- prompt "Enter file path: "
    inputMaybe <- parseInputFromFile filePath
    case inputMaybe of
        Nothing -> putStrLn $ "Could not able to parse the file: " ++ filePath
        Just input -> do
            let part1Ans = solvePart1ForInput input
            putStrLn $ "Part 1: " ++ show part1Ans
