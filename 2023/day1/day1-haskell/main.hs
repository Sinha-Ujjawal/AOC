{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt, isDigit)
import Data.Maybe (isJust)
import Data.Text qualified as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt line = do
  putStr line
  hFlush stdout
  getLine

makeCalibrationValueFunction :: [(T.Text, Int)] -> T.Text -> Int
makeCalibrationValueFunction patterns = go . firstAndLastDigits []
  where
    firstAndLastDigits acc "" = reverse acc
    firstAndLastDigits acc xs =
      case filter (\(pattern, _) -> isJust $ T.stripPrefix pattern xs) patterns of
        [] -> firstAndLastDigits acc (T.tail xs)
        ((_, v) : _) -> firstAndLastDigits (if length acc < 2 then v : acc else v : tail acc) (T.tail xs)

    go [] = 0
    go [x] = x * 11
    go [x,y] = x * 10 + y
    go _ = error "Unreachable!"

solveForFile :: String -> IO (Int, Int)
solveForFile filename =
  let patternsForPart1 =
        [ ("1", 1),
          ("2", 2),
          ("3", 3),
          ("4", 4),
          ("5", 5),
          ("6", 6),
          ("7", 7),
          ("8", 8),
          ("9", 9)
        ]
      calibFnPart1 = makeCalibrationValueFunction patternsForPart1

      patternsForPart2 =
        patternsForPart1
          ++ [ ("one", 1),
               ("two", 2),
               ("three", 3),
               ("four", 4),
               ("five", 5),
               ("six", 6),
               ("seven", 7),
               ("eight", 8),
               ("nine", 9)
             ]
      calibFnPart2 = makeCalibrationValueFunction patternsForPart2
   in do
        input <- T.lines . T.pack <$> readFile filename
        let part1Ans = sum $ fmap calibFnPart1 input
            part2Ans = sum $ fmap calibFnPart2 input
        return (part1Ans, part2Ans)

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  (part1Ans, part2Ans) <- solveForFile filename
  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
