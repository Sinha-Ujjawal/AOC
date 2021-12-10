import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

lookup' :: (Show k, Ord k) => k -> M.Map k a -> a
lookup' k m =
  case M.lookup k m of
    Just a -> a
    Nothing -> error $ "Key: " ++ show k ++ " not found!"

type Grid a = M.Map (Int, Int) a

shape :: Grid a -> (Int, Int)
shape = maybe (0, 0) fst . M.lookupMax

gridFromString :: String -> Grid Int
gridFromString = foldl foldRow M.empty . zip [0 ..] . words
  where
    foldRow m (i, row) = foldl (\m (j, val) -> M.insert (i, j) val m) m (zip [0 ..] $ map (read . (: [])) row)

findLowPoints :: Grid Int -> [((Int, Int), Int)]
findLowPoints grid = [((i, j), lookup' (i, j) grid) | i <- [0 .. rows], j <- [0 .. cols], isLowPoint grid (i, j)]
  where
    (rows, cols) = shape grid

isLowPoint :: Grid Int -> (Int, Int) -> Bool
isLowPoint grid (i, j) =
  case M.lookup (i, j) grid of
    Just val ->
      let adjacentValues = mapMaybe (`M.lookup` grid) $ adjacent (i, j)
       in all (val <) adjacentValues
    Nothing -> False

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

findBasins :: Grid Int -> [[(Int, Int)]]
findBasins grid = snd . foldl folder (S.empty, []) . map fst $ findLowPoints grid
  where
    folder (visited, acc) pos = (visited', acc' : acc)
      where
        (visited', acc') = dfs (visited, []) pos

    dfs (visited, acc) pos =
      if S.member pos visited
        then (visited, acc)
        else case M.lookup pos grid of
          Nothing -> (visited, acc)
          Just 9 -> (visited, acc)
          Just _ ->
            let visited' = S.insert pos visited
             in foldl dfs (visited', pos : acc) $ adjacent pos

solvePart1 :: String -> Int
solvePart1 =
  sum
    . map ((+ 1) . snd)
    . findLowPoints
    . gridFromString

solvePart2 :: String -> Int
solvePart2 =
  product
    . take 3
    . reverse
    . sort
    . map length
    . findBasins
    . gridFromString

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans