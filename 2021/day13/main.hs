{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

readMaybeText :: Read a => T.Text -> Maybe a
readMaybeText = readMaybe . T.unpack

data Fold = AlongX Int | AlongY Int deriving (Show, Eq)

type Points = S.Set (Int, Int)

foldPoints :: Fold -> Points -> Points
foldPoints (AlongX x) = S.fromList . mapMaybe mapX . S.toList
  where
    mapX (x', y)
      | x' < x = Just (x', y)
      | x' == x = Nothing
      | x' > x = Just (x + x - x', y)
foldPoints (AlongY y) = S.fromList . mapMaybe mapY . S.toList
  where
    mapY (x, y')
      | y' < y = Just (x, y')
      | y' == y = Nothing
      | y' > y = Just (x, y + y - y')

showPoints :: Points -> String
showPoints ps = unlines grid
  where
    rows = fromMaybe 0 $ S.lookupMax $ S.map snd ps
    cols = fromMaybe 0 $ S.lookupMax $ S.map fst ps

    grid = [[if S.member (col, row) ps then 'o' else ' ' | col <- [0 .. cols]] | row <- [0 .. rows]]

parseInput :: String -> Maybe (Points, [Fold])
parseInput = go . T.splitOn "\n\n" . T.pack
  where
    go [pointsSpec, foldsSpec] = do
      ps <- points pointsSpec
      fs <- folds foldsSpec
      pure (S.fromList ps, fs)
    go _ = Nothing

    points = traverse (toPoint . T.splitOn ",") . T.lines

    toPoint [x, y] = do
      x' <- readMaybeText x
      y' <- readMaybeText y
      Just (x', y')
    toPoint _ = Nothing

    folds = traverse (toFold . T.words) . T.lines

    toFold ["fold", "along", xy] = toFold' $ T.splitOn "=" xy
    toFold _ = Nothing

    toFold' ["x", x] = AlongX <$> readMaybeText x
    toFold' ["y", y] = AlongY <$> readMaybeText y
    toFold' _ = Nothing

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  (ps, fs) <- parseInput input
  pure $ length $ (!! 1) $ scanl (flip foldPoints) ps fs

solvePart2 :: String -> Maybe String
solvePart2 input = do
  (ps, fs) <- parseInput input
  pure $ showPoints $ foldl (flip foldPoints) ps fs

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: \n" ++ fromMaybe "" part2Ans