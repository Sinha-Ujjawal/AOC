module Main where

import Crypto.Hash (MD5 (..), hashWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import System.IO (hFlush, stdout)

md5Hash :: String -> String
md5Hash = show . hashWith MD5 . BSU.fromString

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

solve :: ((Int, String) -> Bool) -> String -> Int
solve isValid =
  fst
    . head
    . dropWhile (not . isValid)
    . zip [1 ..]
    . repeat

solve' :: Int -> String -> Int
solve' n = solve isValid
  where
    nZeros = replicate n '0'
    isLeadingWithNZeros s = take n s == nZeros
    isValid (number, secret) = isLeadingWithNZeros $ md5Hash (secret ++ show number)

solvePart1 :: String -> Int
solvePart1 = solve' 5

solvePart2 :: String -> Int
solvePart2 = solve' 6

main :: IO ()
main = do
  input <- prompt "Enter input string: "

  let part1Ans = solvePart1 input
      part2Ans = solvePart2 input

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part1Ans
