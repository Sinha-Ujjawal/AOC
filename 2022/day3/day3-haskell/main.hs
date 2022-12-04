{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs =
  let (xs', rest) = L.splitAt n xs
   in xs' : groups n rest

distinctChars :: T.Text -> S.Set Char
distinctChars = S.fromList . T.unpack

priorityBadge :: S.Set Char -> Int
priorityBadge = S.fold (+) 0 . S.map priority
  where
    priority :: Char -> Int
    priority c
      | Char.isAlpha c =
        if Char.isLower c
          then Char.ord c - Char.ord 'a' + 1
          else Char.ord c - Char.ord 'A' + 27
      | otherwise = error $ "Characters can only be alphabets! found: " ++ show c

badgeRuskSacks :: [T.Text] -> S.Set Char
badgeRuskSacks [] = S.empty
badgeRuskSacks (x : rest) = L.foldl' S.intersection (distinctChars x) (fmap distinctChars rest)

priorityRuskSacks :: [T.Text] -> Int
priorityRuskSacks = priorityBadge . badgeRuskSacks

solve :: Int -> [T.Text] -> Int
solve n = sum . fmap priorityRuskSacks . groups n

solvePart1 :: T.Text -> Int
solvePart1 =
  solve 2
    . (=<<) splitHalf
    . T.lines
    . T.strip
  where
    splitHalf str = let (l, r) = T.splitAt (T.length str `div` 2) str in [l, r]

solvePart2 :: T.Text -> Int
solvePart2 =
  solve 3
    . T.lines
    . T.strip

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  contents <- T.pack <$> readFile filePath
  let part1Ans = solvePart1 contents
  putStrLn $ "Part 1: " ++ show part1Ans
  let part2Ans = solvePart2 contents
  putStrLn $ "Part 2: " ++ show part2Ans
