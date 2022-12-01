{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import qualified Data.Ord as Ord
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

type Calorie = Int

type Calories = [Calorie]

type ElvesCalories = [Calories]

parseElvesCalories :: T.Text -> Maybe ElvesCalories
parseElvesCalories = traverse parseElvesCalories' . T.splitOn "\n\n" . T.stripEnd
  where
    -- parseElvesCalories' :: Integral a => T.Text -> Maybe [a]
    parseElvesCalories' = traverse (readMaybe . T.unpack) . T.splitOn "\n"

solvePart1 :: ElvesCalories -> Calorie
solvePart1 [] = 0
solvePart1 xs = maximum $ sum <$> xs

solvePart2 :: ElvesCalories -> Calorie
solvePart2 = sum . take 3 . reverse . sort . fmap sum

main :: IO ()
main = do
  filename <- prompt "Enter file path: "
  contents <- T.pack <$> readFile filename
  case parseElvesCalories contents of
    Nothing -> error $ "Error parsing the calories from the file" ++ filename
    Just numbers -> do
      let part1Ans = solvePart1 numbers
          part2Ans = solvePart2 numbers
      putStrLn $ "Part 1: " ++ show part1Ans
      putStrLn $ "Part 2: " ++ show part2Ans
