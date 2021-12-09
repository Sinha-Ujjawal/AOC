import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

groupBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
groupBy fun = foldl (\m a -> M.insertWith (++) (fun a) [a] m) M.empty

solvePart1 :: [String] -> Int
solvePart1 = sum . map (solve1478 . words)
  where
    solve1478 = length . filter (flip S.member (S.fromList [2, 4, 3, 7]) . length) . drop 11

solvePart2 :: [String] -> Int
solvePart2 = sum . map (solve . filter (/= "|") . words)
  where
    solve digits = (a * 1000) + (b * 100) + (c * 10) + d
      where
        m = M.map (map S.fromList) . groupBy length $ take 10 digits

        lup k = fromMaybe [] . M.lookup k

        [_1] = lup 2 m
        [_4] = lup 4 m
        [_7] = lup 3 m
        [_8] = lup 7 m

        [_2] = filter ((== 3) . length . flip S.difference _4) $ lup 5 m
        ([_3], [_5]) = partition ((== _1) . S.union _1 . flip S.difference _2) . filter (/= _2) $ lup 5 m

        _9 = S.union _1 _5
        ([_6], [_0]) = partition ((== _8) . S.union _1) . filter (/= _9) $ lup 6 m

        m' = M.fromList [(_0, 0), (_1, 1), (_2, 2), (_3, 3), (_4, 4), (_5, 5), (_6, 6), (_7, 7), (_8, 8), (_9, 9)]

        [a, b, c, d] = mapMaybe (flip M.lookup m' . S.fromList) $ drop 10 digits

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  lines <- lines <$> readFile filename

  let part1Ans = solvePart1 lines
      part2Ans = solvePart2 lines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans