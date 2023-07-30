import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

data Direction = U | D | L | R deriving (Show, Eq)

parse :: String -> Maybe [Direction]
parse = traverse go
  where
    go '^' = Just U
    go '<' = Just L
    go 'v' = Just D
    go '>' = Just R
    go _ = Nothing

type Position = (Int, Int)

updatePosition :: Position -> Direction -> Position
updatePosition (x, y) U = (x, y - 1)
updatePosition (x, y) D = (x, y + 1)
updatePosition (x, y) L = (x - 1, y)
updatePosition (x, y) R = (x + 1, y)

distinctVisits :: [Direction] -> S.Set Position
distinctVisits = S.fromList . scanl updatePosition (0, 0)

solvePart1 :: [Direction] -> Int
solvePart1 = S.size . distinctVisits

solvePart2 :: [Direction] -> Int
solvePart2 dirs = S.size $ S.union (distinctVisits santaDirections) (distinctVisits santaBotDirections)
  where
    dirsWithIndex = zip [0 ..] dirs
    santaDirections = map snd $ filter (even . fst) dirsWithIndex
    santaBotDirections = map snd $ filter (odd . fst) dirsWithIndex

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  firstLineInFile <- head . lines <$> readFile filename

  let input = fromMaybe [] $ parse firstLineInFile
      part1Ans = solvePart1 input
      part2Ans = solvePart2 input

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
