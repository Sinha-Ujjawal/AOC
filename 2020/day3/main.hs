import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

descend :: (Int, Int) -> [[a]] -> [a]
descend (right, down)
  | right > 0 && down > 0 = go 0
  | otherwise = const []
  where
    go i [] = []
    go i ([] : xss) = go i xss
    go i (xs : xss) = (xs !! j) : go (j + right) (drop (down - 1) xss)
      where
        j = rem i (length xs)

solver :: (Int, Int) -> [String] -> Int
solver slope = length . filter (== '#') . descend slope

solvePart1 :: [String] -> Int
solvePart1 = solver (3, 1)

solvePart2 :: [String] -> Int
solvePart2 arena = product [solver slope arena | slope <- slopes]
  where
    slopes =
      [ (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
      ]

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans