{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

type Item2Count a = M.Map a Integer

type Count2Items a = M.Map Integer (S.Set a)

data Bag a = Bag {item2Count :: Item2Count a, count2Items :: Count2Items a} deriving (Show, Eq)

emptyBag :: Ord a => Bag a
emptyBag = Bag M.empty M.empty

upsertItem2Count' :: Ord a => a -> Integer -> Item2Count a -> Item2Count a
upsertItem2Count' a n m
  | n <= 0 = M.delete a m
  | otherwise = M.insert a n m

upsertCount2Items' :: Ord a => Integer -> S.Set a -> Count2Items a -> Count2Items a
upsertCount2Items' cnt items m
  | null items || cnt <= 0 = M.delete cnt m
  | otherwise = M.insert cnt items m

insertMany :: Ord a => Integer -> a -> Bag a -> Bag a
insertMany n a bag@(Bag m m') = Bag m'' m'''
  where
    oldCount = fromMaybe 0 $ M.lookup a m
    oldCountSet = S.delete a $ fromMaybe S.empty $ M.lookup oldCount m'

    newCount = oldCount + n
    newCountSet = S.insert a $ fromMaybe S.empty $ M.lookup newCount m'

    m'' = upsertItem2Count' a newCount m
    m''' = upsertCount2Items' newCount newCountSet $ upsertCount2Items' oldCount oldCountSet m'

insert :: Ord a => a -> Bag a -> Bag a
insert = insertMany 1

deleteMany :: Ord a => Integer -> a -> Bag a -> Bag a
deleteMany n = insertMany (-n)

delete :: Ord a => a -> Bag a -> Bag a
delete = deleteMany 1

mostCommonElements :: Ord a => Bag a -> Maybe (Integer, S.Set a)
mostCommonElements (Bag _ m) = M.lookupMax m

leastCommonElements :: Ord a => Bag a -> Maybe (Integer, S.Set a)
leastCommonElements (Bag _ m) = M.lookupMin m

----------------------------------------------------------------------------------------------------------------------

type Polymer = (Item2Count (Char, Char), Bag Char)

type Rules = M.Map (Char, Char) Char

toPolymer :: String -> Polymer
toPolymer polymer = (pairCounts, bag)
  where
    pairCounts = foldl (\m a -> M.insertWith (+) a 1 m) M.empty $ zip polymer (tail polymer)
    bag = foldl (flip insert) emptyBag polymer

parseInput :: String -> Maybe (Polymer, Rules)
parseInput = go . T.splitOn "\n\n" . T.pack
  where
    go [polymer, rulesSpec] = do
      rules <- parseRules rulesSpec
      Just (toPolymer . T.unpack $ polymer, M.fromList rules)
    go _ = Nothing

    parseRules = traverse (parseRule . map T.unpack . T.splitOn " -> ") . T.lines

    parseRule [[a, b], [c]] = Just ((a, b), c)
    parseRule _ = Nothing

nextPolymer :: Rules -> Polymer -> Polymer
nextPolymer rules (pairCounts, bag) = M.foldrWithKey folder (M.empty, bag) pairCounts
  where
    folder charpair@(a, b) cnt (m, bag) =
      case M.lookup charpair rules of
        Just c -> (M.insertWith (+) (a, c) cnt $ M.insertWith (+) (c, b) cnt m, insertMany cnt c bag)
        Nothing -> (M.insertWith (+) charpair cnt m, bag)

nextPolymers :: Rules -> Polymer -> [Polymer]
nextPolymers rules = iterate (nextPolymer rules)

solver :: Int -> String -> Maybe Integer
solver n input = do
  (polymer, rules) <- parseInput input
  let (_, bag) = (!! n) $ nextPolymers rules polymer
  (maxCount, _) <- mostCommonElements bag
  (minCount, _) <- leastCommonElements bag
  pure (maxCount - minCount)

solvePart1 :: String -> Maybe Integer
solvePart1 = solver 10

solvePart2 :: String -> Maybe Integer
solvePart2 = solver 40

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans