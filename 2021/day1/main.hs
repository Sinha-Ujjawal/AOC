import Data.List (tails)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

changes :: Ord a => [a] -> [Bool]
changes = zipWith (>) <$> safeTail <*> id

rollingSums :: Num a => Int -> [a] -> Maybe [a]
rollingSums n xs
  | n <= 0 = Nothing
  | otherwise = Just $ if length as == n then go (sum as) xs bs else []
  where
    (as, bs) = splitAt n xs

    go s xs [] = [s]
    go s xs (y : ys) = s : go (s + y - head xs) (tail xs) ys

solvePart1 :: Integral a => [a] -> Int
solvePart1 = length . filter id . changes

solvePart2 :: Integral a => [a] -> Int
solvePart2 = solvePart1 . fromMaybe [] . rollingSums 3

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- map read . lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans
