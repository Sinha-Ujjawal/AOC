import Data.List (nub, tails)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

solve :: Int -> String -> Maybe Int
solve n =
  fmap ((+ n) . fst)
    . safeHead
    . dropWhile (not . snd)
    . zip [0 ..]
    . map ((\xs -> nub xs == xs) . take n)
    . tails

solvePart1 :: [String] -> [Maybe Int]
solvePart1 = map $ solve 4

solvePart2 :: [String] -> [Maybe Int]
solvePart2 = map $ solve 14

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  contents <- readFile filePath
  let part1Ans = solvePart1 $ lines contents
  putStrLn $ "Part 1: " ++ show part1Ans
  let part2Ans = solvePart2 $ lines contents
  putStrLn $ "Part 2: " ++ show part2Ans