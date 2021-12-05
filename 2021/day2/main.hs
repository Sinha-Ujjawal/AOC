import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

data Move
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show, Eq)

parseMove :: String -> Maybe Move
parseMove = toCMD . parseCMDIntPair . words
  where
    parseCMDIntPair [cmd, x] = Just (cmd, readMaybe x)
    parseCMDIntPair _ = Nothing

    toCMD (Just ("forward", Just x)) = Just $ Forward x
    toCMD (Just ("up", Just x)) = Just $ Up x
    toCMD (Just ("down", Just x)) = Just $ Down x
    toCMD _ = Nothing

parseMoves :: [String] -> [Move]
parseMoves = mapMaybe parseMove

solver :: (a -> Move -> a) -> (a -> b) -> a -> [String] -> b
solver interpreter resolver initial =
  resolver
    . foldl interpreter initial
    . parseMoves

solvePart1 :: [String] -> Int
solvePart1 = solver interpreter (uncurry (*)) (0, 0)
  where
    interpreter (pos, depth) move =
      case move of
        Forward x -> (pos + x, depth)
        Up x -> (pos, depth - x)
        Down x -> (pos, depth + x)

solvePart2 :: [String] -> Int
solvePart2 = solver interpreter (\(x, y, _) -> x * y) (0, 0, 0)
  where
    interpreter (pos, depth, aim) move =
      case move of
        Forward x -> (pos + x, depth + aim * x, aim)
        Up x -> (pos, depth, aim - x)
        Down x -> (pos, depth, aim + x)

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans