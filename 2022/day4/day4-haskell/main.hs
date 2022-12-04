{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

data MyError = ParseError {parseErrorLineNo :: !Int, parseErrorLine :: !T.Text} deriving (Show)

parseInput :: T.Text -> Either MyError [((Int, Int), (Int, Int))]
parseInput = traverse parseIntervalPair . zip [1 ..] . T.lines
  where
    parseIntervalPair lineContext@(lineNo, line) = do
      case T.splitOn "," line of
        [leftIntervalStr, rightIntervalStr] -> parseIntervalPair' lineContext leftIntervalStr rightIntervalStr
        _otherwise -> Left $ ParseError lineNo line

    parseIntervalPair' lineContext@(lineNo, line) leftIntervalStr rightIntervalStr = do
      leftInterval <- parseInterval lineContext leftIntervalStr
      rightInterval <- parseInterval lineContext rightIntervalStr
      return (leftInterval, rightInterval)

    parseInterval lineContext@(lineNo, line) intervalStr = do
      case T.unpack <$> T.splitOn "-" intervalStr of
        [readMaybe -> Just intervalStart, readMaybe -> Just intervalEnd] -> Right (intervalStart, intervalEnd)
        _otherwise -> Left $ ParseError lineNo line

intervalsCompletelyContains :: (Int, Int) -> (Int, Int) -> Bool
intervalsCompletelyContains (ll, lh) (rl, rh)
  | ll <= lh && rl <= rh = (rl >= ll && rh <= lh) || (ll >= rl && lh <= rh)
  | otherwise = False

intervalsOverlap :: (Int, Int) -> (Int, Int) -> Bool
intervalsOverlap (ll, lh) (rl, rh)
  | ll <= lh && rl <= rh = (ll <= rh && rl <= lh) || (rl <= lh && ll <= rh)
  | otherwise = False

solvePart1 :: [((Int, Int), (Int, Int))] -> Int
solvePart1 = length . filter (uncurry intervalsCompletelyContains)

solvePart2 :: [((Int, Int), (Int, Int))] -> Int
solvePart2 = length . filter (uncurry intervalsOverlap)

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  contents <- T.pack <$> readFile filePath
  case parseInput contents of
    Left err -> error (show err)
    Right parsedInput -> do
      let part1Ans = solvePart1 parsedInput
      putStrLn $ "Part 1: " ++ show part1Ans
      let part2Ans = solvePart2 parsedInput
      putStrLn $ "Part 2: " ++ show part2Ans
