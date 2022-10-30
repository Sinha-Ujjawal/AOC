import Data.List (isInfixOf, sort, tails)
import System.IO (hFlush, stdout)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

containsConseqDuplicate :: Eq a => [a] -> Bool
containsConseqDuplicate [] = False
containsConseqDuplicate xs = any (uncurry (==)) (zip xs (tail xs))

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

solve :: (String -> Bool) -> [String] -> Int
solve isNice = length . filter isNice

solvePart1 :: [String] -> Int
solvePart1 = solve isNice
  where
    isNice :: String -> Bool
    isNice "" = False
    isNice s = vowelCount >= 3 && containsConseqDuplicate s && doesNotContain
      where
        vowelCount = length $ filter isVowel s

        doesNotContain = not $ any go (zip s (tail s))
        go ('a', 'b') = True
        go ('c', 'd') = True
        go ('p', 'q') = True
        go ('x', 'y') = True
        go _ = False

solvePart2 :: [String] -> Int
solvePart2 = solve isNice
  where
    isNice :: String -> Bool
    isNice s = satisfiesFirstCiteria s && satisfiesSecondCriteria s

    satisfiesFirstCiteria (x : y : rest) = isInfixOf [x, y] rest || satisfiesFirstCiteria (y : rest)
    satisfiesFirstCiteria _ = False

    satisfiesSecondCriteria = any (go . take 3) . tails
      where
        go [x, _, y] = x == y
        go _ = False

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  strings <- lines <$> readFile filename

  let part1Ans = solvePart1 strings
      part2Ans = solvePart2 strings

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
