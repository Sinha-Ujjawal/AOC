import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

fuel :: Integral a => a -> a
fuel = flip (-) 2 . flip div 3

solvePart1 :: Integral a => [a] -> a
solvePart1 = sum . map fuel

solvePart2 :: Integral a => [a] -> a
solvePart2 = sum . fmap (sum . takeWhile (>= 0) . tail . iterate fuel)

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- map read . lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans
