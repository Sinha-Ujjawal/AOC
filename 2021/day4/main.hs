{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join)
import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable (traverse)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

takeEvery :: Int -> [a] -> Maybe [[a]]
takeEvery n xs
  | n <= 0 = Nothing
  | otherwise = Just $ go xs
  where
    go [] = []
    go xs = ((:) <$> fst <*> (go . snd)) $ splitAt n xs

takeEveryStrict :: Int -> [a] -> Maybe [[a]]
takeEveryStrict n = fmap (filter ((== n) . length)) . takeEvery n

data Cell = Marked Int | Unmarked Int deriving (Show, Eq)

cellValue :: Cell -> Int
cellValue (Marked x) = x
cellValue (Unmarked x) = x

isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked _ = False

type Board = (Int, [[Cell]])

markBoard :: Int -> Board -> Board
markBoard val (id, board) = (id, map (map (\cell -> if cellValue cell == val then Marked val else cell)) board)

unmarkedValues :: Board -> [Int]
unmarkedValues = map cellValue . filter (not . isMarked) . join . snd

isBoardWon :: Board -> Bool
isBoardWon (_, board) = any (all isMarked) (board ++ transpose board)

plays :: [Board] -> [Int] -> [[Board]]
plays boards = tail . scanl (\boards input -> map (markBoard input) boards) boards

firstWins :: [Board] -> [Int] -> [[Board]]
firstWins boards = map snd . tail . scanl resolver (S.empty, []) . plays boards
  where
    resolver (s, _) boards = (S.union s (S.fromList $ map fst winningBoards), winningBoards)
      where
        winningBoards = filter ((&&) <$> (not . flip S.member s . fst) <*> isBoardWon) boards

parseInputs :: [String] -> Maybe [Int]
parseInputs =
  traverse (readMaybe . T.unpack)
    . T.splitOn ","
    . T.pack
    . fromMaybe ""
    . safeHead

parseBoards :: [String] -> Maybe [Board]
parseBoards =
  fmap (zip [1 ..])
    . (takeEveryStrict 5 =<<)
    . (takeEveryStrict 5 =<<)
    . traverse (fmap Unmarked . readMaybe)
    . drop 1

parse :: [String] -> Maybe ([Int], [Board])
parse words = do
  inputs <- parseInputs words
  boards <- parseBoards words
  pure (inputs, boards)

solvePart1 :: [String] -> Maybe Int
solvePart1 words = do
  (inputs, boards) <- parse words
  let playsWithWinnerBoard =
        filter ((/= []) . snd) $
          zip inputs $
            map (filter isBoardWon) $
              plays boards inputs
  case playsWithWinnerBoard of
    ((input, board : _) : _) -> Just $ input * sum (unmarkedValues board)
    _ -> Nothing

solvePart2 :: [String] -> Maybe Int
solvePart2 words = do
  (inputs, boards) <- parse words
  let playsWithWinnerBoard =
        filter ((/= []) . snd) $
          zip inputs $
            firstWins boards inputs
  case safeLast playsWithWinnerBoard of
    Just (input, board : _) -> Just $ input * sum (unmarkedValues board)
    _ -> Nothing

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileWords <- words <$> readFile filename

  let part1Ans = solvePart1 fileWords
      part2Ans = solvePart2 fileWords

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans