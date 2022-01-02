{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_)
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

parseInput :: String -> Maybe ((Int, Int), (Int, Int))
parseInput = go . T.pack
  where
    go (T.stripPrefix "target area: " -> Just rest) = go' $ T.splitOn ", " rest
    go _ = Nothing

    go' [T.stripPrefix "x=" -> Just xs, T.stripPrefix "y=" -> Just ys] = go'' (readMaybeText <$> T.splitOn ".." xs) (readMaybeText <$> T.splitOn ".." ys)
    go' _ = Nothing

    go'' [Just xl, Just xu] [Just yl, Just yu] = Just ((xl, xu), (yl, yu))
    go'' _ _ = Nothing

findMaxHeight :: (Int, Int) -> Int
findMaxHeight (yl, yu) = (n * (n + 1)) `div` 2
  where
    n = max (go yl) (go yu)

    go y
      | y < 0 = -y - 1
      | otherwise = y

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  ((_, _), (yl, yu)) <- parseInput input
  pure $ findMaxHeight (yl, yu)

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  ls <- lines <$> readFile filename

  putStrLn "Part 1:"
  forM_ ls (\l -> putStrLn $ l ++ " => " ++ show (solvePart1 l))
