{-# LANGUAGE OverloadedStrings #-}

import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

parseNumbers :: (Read a, Num a) => T.Text -> Maybe [a]
parseNumbers = traverse (readMaybe . T.unpack) . filter (/= "") . T.split isSpace

newtype PuzzleInput = PuzzleInput { timesAndDistances :: [(Int, Int)] } deriving (Show, Eq)

parsePuzzleInputFromString :: T.Text -> Maybe PuzzleInput
parsePuzzleInputFromString = go . T.lines
  where
    go [timesStr, distsStr] = do
        times <- parseTimes timesStr
        distances <- parseDistances distsStr
        return $ PuzzleInput { timesAndDistances = zip times distances }
    go _ = Nothing

    parseTimes :: T.Text -> Maybe [Int]
    parseTimes = parseNumbers <=< T.stripPrefix "Time:"

    parseDistances :: T.Text -> Maybe [Int]
    parseDistances = parseNumbers <=< T.stripPrefix "Distance:"

parsePuzzleInputFromFile :: String -> IO (Maybe PuzzleInput)
parsePuzzleInputFromFile = fmap parsePuzzleInputFromString . T.readFile

numPossibilities :: Int -> Int -> Int
numPossibilities n t =
    let n'   = fromIntegral n :: Double
        t'   = fromIntegral t :: Double
        off  = sqrt (n'*n' - 4*t')
        up   = (n' + off) / 2
        low  = (n' - off) / 2
        up'  = case properFraction up of
                   (_, 0) -> floor up - 1
                   _      -> floor up
        low' = case properFraction low of
                   (_, 0) -> ceiling low + 1
                   _      -> ceiling low
    in up' - low' + 1

solvePart1ForPuzzleInput :: PuzzleInput -> Int
solvePart1ForPuzzleInput = product . fmap (uncurry numPossibilities) . timesAndDistances

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    puzzleInputMaybe <- parsePuzzleInputFromFile filePath
    case puzzleInputMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just puzzleInput ->
            return $ solvePart1ForPuzzleInput puzzleInput

solvePart2ForPuzzleInput :: PuzzleInput -> Int
solvePart2ForPuzzleInput =
    uncurry numPossibilities
    . (\xs -> (read (concatMap (show . fst) xs), read (concatMap (show . snd) xs)))
    . timesAndDistances

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    puzzleInputMaybe <- parsePuzzleInputFromFile filePath
    case puzzleInputMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just puzzleInput ->
            return $ solvePart2ForPuzzleInput puzzleInput

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    puzzleInputMaybe <- parsePuzzleInputFromFile filePath
    case puzzleInputMaybe of
        Nothing -> putStrLn $ "Could not able to parse the file: " ++ filePath
        Just puzzleInput -> do
            let part1Ans = solvePart1ForPuzzleInput puzzleInput
                part2Ans = solvePart2ForPuzzleInput puzzleInput
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
