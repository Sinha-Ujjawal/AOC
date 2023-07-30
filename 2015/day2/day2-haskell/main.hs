import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

stripLeft :: String -> String
stripLeft = dropWhile (\c -> c == ' ' || c == '\t')

strip :: String -> String
strip = reverse . stripLeft . reverse . stripLeft

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = left : splitOn d (safeTail restWithSep)
  where
    (left, restWithSep) = span (/= d) s

parseInput :: String -> Maybe [(Int, Int, Int)]
parseInput = traverse (parseLine . strip) . lines
  where
    parseLine = parse . splitOn 'x'
    parse [l, w, h] = do
      l' <- readMaybe l
      w' <- readMaybe w
      h' <- readMaybe h
      return (l', w', h')
    parse _ = Nothing

solve :: ((Int, Int, Int) -> Int) -> [(Int, Int, Int)] -> Int
solve areaCalculator = sum . map areaCalculator

calculateWrappingPaperArea :: (Int, Int, Int) -> Int
calculateWrappingPaperArea (l, w, h) = (2 * s1 + 2 * s2 + 2 * s3) + minimum [s1, s2, s3]
  where
    s1 = l * w
    s2 = w * h
    s3 = l * h

solvePart1 :: [(Int, Int, Int)] -> Int
solvePart1 = solve calculateWrappingPaperArea

calculateRibbonFeet :: (Int, Int, Int) -> Int
calculateRibbonFeet (l, w, h) = l * w * h + 2 * minimum [l + w, w + h, l + h]

solvePart2 :: [(Int, Int, Int)] -> Int
solvePart2 = solve calculateRibbonFeet

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileContents <- readFile filename

  let input = fromMaybe [] $ parseInput fileContents
      part1Ans = solvePart1 input
      part2Ans = solvePart2 input

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
