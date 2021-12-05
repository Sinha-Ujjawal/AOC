import Data.List (tails)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

twoSum :: (Ord a, Num a) => a -> [a] -> Maybe (a, a)
twoSum target = go S.empty
  where
    go s [] = Nothing
    go s (x : xs) =
      let y = target - x
       in if S.member y s
            then Just (x, y)
            else go (S.insert x s) xs

threeSum :: (Ord a, Num a) => a -> [a] -> Maybe (a, a, a)
threeSum target = safeHead . mapMaybe go . tails
  where
    go [] = Nothing
    go (x : xs) =
      case twoSum (target - x) xs of
        Just (y, z) -> Just (x, y, z)
        Nothing -> Nothing

solvePart1 :: [Int] -> Maybe Int
solvePart1 = fmap (uncurry (*)) . twoSum 2020

solvePart2 :: [Int] -> Maybe Int
solvePart2 = fmap (\(x, y, z) -> x * y * z) . threeSum 2020

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- map read . lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans
