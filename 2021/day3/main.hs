import Data.List (transpose)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

shape :: [[a]] -> Maybe (Int, Int)
shape xss = if length lengths == 1 then Just (rows, head lengths) else Nothing
  where
    rows = length xss
    lengths = ensureAtmostOne $ map length xss

    ensureAtmostOne [] = []
    ensureAtmostOne [x1, x2] = [x1 | x1 == x2]
    ensureAtmostOne (x1 : x2 : xs) = if x1 == x2 then ensureAtmostOne (x2 : xs) else []

data Bit = Z | O deriving (Show, Eq)

type Bin = [Bit]

readBinMaybe :: String -> Maybe [Bit]
readBinMaybe "" = Just []
readBinMaybe ('0' : xs) = (Z :) <$> readBinMaybe xs
readBinMaybe ('1' : xs) = (O :) <$> readBinMaybe xs
readBinMaybe _ = Nothing

mostCommonBit :: Bin -> Bit
mostCommonBit xs =
  if oneCount + oneCount >= l then O else Z
  where
    l = length xs
    oneCount = length $ filter (== O) xs

leastCommonBit :: Bin -> Bit
leastCommonBit = complementBit . mostCommonBit

complementBit :: Bit -> Bit
complementBit Z = O
complementBit O = Z

complement :: Bin -> Bin
complement = map complementBit

toDecimal :: Bin -> Int
toDecimal = go 0
  where
    go acc [] = acc
    go acc (x : xs) =
      case x of
        Z -> go (acc + acc) xs
        O -> go (acc + acc + 1) xs

solvePart1 :: [String] -> Int
solvePart1 =
  ((*) <$> toDecimal <*> toDecimal . complement)
    . map mostCommonBit
    . transpose
    . mapMaybe readBinMaybe

solver :: (Bin -> [Bool]) -> [Bin] -> Maybe Int
solver criteria bs =
  case shape bs of
    Just (_, _) -> go bs bs
    Nothing -> Nothing
  where
    go [] _ = Nothing
    go [b] _ = Just $ toDecimal b
    go bs bs' =
      let msk = criteria $ map head bs'
          bs'' = map snd $ filter fst $ zip msk bs
          bs''' = map snd $ filter fst $ zip msk bs'
       in go bs'' (map tail bs''')

solver' :: (Bin -> [Bool]) -> [Bin] -> Int
solver' criteria = fromMaybe 0 . solver criteria

oxygenGenRating :: Bin -> [Bool]
oxygenGenRating b = map (== mostCommonBit b) b

oxygenSolver :: [Bin] -> Int
oxygenSolver = solver' oxygenGenRating

co2ScrubberRating :: Bin -> [Bool]
co2ScrubberRating b = map (== leastCommonBit b) b

co2ScrubberSolver :: [Bin] -> Int
co2ScrubberSolver = solver' co2ScrubberRating

solvePart2 :: [String] -> Int
solvePart2 =
  ((*) <$> oxygenSolver <*> co2ScrubberSolver)
    . mapMaybe readBinMaybe

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans