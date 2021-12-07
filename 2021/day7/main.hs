{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- O(n lg n)
median :: (Ord a, Num a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median xs =
  Just $
    if even l
      then ((xs' !! l') + (xs' !! (l' + 1))) / 2
      else xs' !! l'
  where
    l = length xs
    l' = div l 2
    xs' = sort xs

mean :: (Num a, Fractional a) => [a] -> Maybe a
mean [] = Nothing
mean xs = Just (sum xs / fromIntegral (length xs))

costs :: Num a => (a -> a -> a) -> [a] -> a -> [a]
costs costFn xs y = [costFn x y | x <- xs]

cost :: Num a => (a -> a -> a) -> [a] -> a -> a
cost costFn xs = sum . costs costFn xs

solver :: (Ord a, Num a) => (a -> a -> a) -> [a] -> [a] -> a
solver costFn xs ys = minimum [cost costFn xs y | y <- ys]

solvePart1 :: [Int] -> Int
solvePart1 xs = solver (\x y -> abs (x - y)) xs xs

solvePart1' :: [Int] -> Int
solvePart1' xs = solver (\x y -> abs (x - y)) xs [tgt - 1, tgt, tgt + 1]
  where
    tgt = floor . fromMaybe 0.0 . median $ map fromIntegral xs

sum1ToN :: Int -> Int
sum1ToN n = div (n * (n + 1)) 2

solvePart2 :: [Int] -> Int
solvePart2 xs = solver (\x y -> sum1ToN (abs (x - y))) xs [minimum xs .. maximum xs]

solvePart2' :: [Int] -> Int
solvePart2' xs = solver (\x y -> sum1ToN (abs (x - y))) xs [tgt - 1, tgt, tgt + 1]
  where
    tgt = floor . fromMaybe 0.0 . mean $ map fromIntegral xs

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  nums <- map (read . T.unpack) . T.splitOn "," . T.pack <$> readFile filename

  let part1Ans = solvePart1 nums
      part1Ans' = solvePart1' nums
      part2Ans = solvePart2 nums
      part2Ans' = solvePart2' nums

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part1 (Another Solution): " ++ show part1Ans'
  putStrLn $ "Part2: " ++ show part2Ans
  putStrLn $ "Part2 (Another Solution): " ++ show part2Ans'