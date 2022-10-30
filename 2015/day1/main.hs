import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

charToInt :: Char -> Int
charToInt '(' = 1
charToInt ')' = -1
charToInt _ = 0

solvePart1 :: String -> Int
solvePart1 = sum . map charToInt

solvePart2 :: String -> Int
solvePart2 =
  fst
    . head
    . dropWhile (\(_, p) -> p >= 0)
    . zip [1 ..]
    . tail
    . scanl (+) 0
    . map charToInt

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  line <- head . lines <$> readFile filename

  let part1Ans = solvePart1 line
      part2Ans = solvePart2 line

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
