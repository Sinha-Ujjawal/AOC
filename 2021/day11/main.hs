{-# LANGUAGE TupleSections #-}

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

type Grid a = M.Map (Int, Int) a

shapeGrid :: Grid a -> (Int, Int)
shapeGrid = maybe (0, 0) fst . M.lookupMax

lookup' :: (Ord a, Show a) => a -> M.Map a b -> b
lookup' a m =
  case M.lookup a m of
    Just b -> b
    Nothing -> error $ "Key: " ++ show a ++ " not found!"

toGrid :: String -> Grid Int
toGrid = foldl foldRow M.empty . zip [0 ..] . words
  where
    foldRow m (i, row) = foldl (\m (j, val) -> M.insert (i, j) (read [val]) m) m $ zip [0 ..] row

showGrid :: Show a => Grid a -> String
showGrid grid = unlines $ map (unwords . map show) values
  where
    (rows, cols) = shapeGrid grid
    values = [[lookup' (i, j) grid | j <- [0 .. cols]] | i <- [0 .. rows]]

showGrids :: Show a => [Grid a] -> String
showGrids = intercalate "\n\n" . map showGrid

simulate :: Grid Int -> [(Int, Grid Int)]
simulate = iterate incrementValues . (0,)
  where
    incrementValues (flashes, grid) = popNines flashes (M.map (+ 1) grid)

    popNines flashes grid =
      case nines of
        [] -> (flashes, grid)
        (pos : _) ->
          let grid' = foldl (flip (M.update (Just . (+ 1)))) (M.insert pos 0 grid) $ adjacent pos
              (flashes', grid'') = popNines (flashes + 1) grid'
           in (flashes', M.insert pos 0 grid'')
      where
        nines = findNines grid

    findNines = map fst . M.toList . M.filter (>= 10)

    adjacent (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1), (i + 1, j + 1), (i + 1, j - 1), (i - 1, j + 1), (i - 1, j - 1)]

solvePart1 :: String -> Int
solvePart1 = fst . (!! 100) . simulate . toGrid

solvePart2 :: String -> Int
solvePart2 = fst . head . filter (allZero . snd . snd) . zip [0 ..] . simulate . toGrid
  where
    allZero = null . M.filter (/= 0)

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans