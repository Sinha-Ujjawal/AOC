{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

counts :: (Ord a, Integral b) => [a] -> M.Map a b
counts = foldl (\m a -> M.insertWith (+) a 1 m) M.empty

lanternfish :: [Int] -> [[Int]]
lanternfish = iterate go
  where
    go = foldl go' []

    go' acc 0 = 8 : 6 : acc
    go' acc f = (f - 1) : acc

lanternfishCompact :: M.Map Int Int -> [M.Map Int Int]
lanternfishCompact = iterate go
  where
    go = M.foldlWithKey go' M.empty

    go' m 0 fishCount = (M.insertWith (+) 8 fishCount . M.insertWith (+) 6 fishCount) m
    go' m t fishCount = M.insertWith (+) (t - 1) fishCount m

solver :: Int -> [Int] -> Int
solver days = sum . (!! max 0 days) . lanternfishCompact . counts

solvePart1 :: [Int] -> Int
solvePart1 = solver 80

solvePart2 :: [Int] -> Int
solvePart2 = solver 256

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  nums <- map (read . T.unpack) . T.splitOn "," . T.pack <$> readFile filename

  let part1Ans = solvePart1 nums
      part2Ans = solvePart2 nums

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans
