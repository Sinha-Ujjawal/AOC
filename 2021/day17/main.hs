{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

takeWhileWithThreshold :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileWithThreshold thresh match = go 0
  where
    go t [] = []
    go t (x : xs) = let b = match x in if b || t < thresh then x : go (if b then t else t + 1) xs else []

readMaybeText :: Read a => T.Text -> Maybe a
readMaybeText = readMaybe . T.unpack

parseInput :: String -> Maybe ((Int, Int), (Int, Int))
parseInput = go . T.pack
  where
    go (T.stripPrefix "target area: " -> Just rest) = go' $ T.splitOn ", " rest
    go _ = Nothing

    go' [T.stripPrefix "x=" -> Just xs, T.stripPrefix "y=" -> Just ys] = go'' (readMaybeText <$> T.splitOn ".." xs) (readMaybeText <$> T.splitOn ".." ys)
    go' _ = Nothing

    go'' [Just xl, Just xu] [Just yl, Just yu] = Just ((xl, xu), (yl, yu))
    go'' _ _ = Nothing

findMaxVelocity :: Int -> Int -> Int
findMaxVelocity yl yu = max (go yl) (go yu)
  where
    go y
      | y < 0 = -y - 1
      | otherwise = y

findMaxHeight :: (Int, Int) -> Int
findMaxHeight (yl, yu) = (n * (n + 1)) `div` 2
  where
    n = findMaxVelocity yl yu

findTimes :: Int -> Int -> [Double]
findTimes u s = filter (>= 0) $ filter (not . isNaN) [(a + b) / 2, (a - b) / 2]
  where
    a = fromIntegral $ 1 + u + u
    b = sqrt (fromIntegral $ 1 + 4 * ((u * u) + u - s - s))

findTime :: Int -> Int -> Maybe Int
findTime u s =
  if null ts then Nothing else Just $ floor $ minimum ts
  where
    ts = findTimes u s

findDisplacement :: Int -> Int -> Int
findDisplacement u t = (u * t) - ((t * (t - 1)) `div` 2)

findReachableDisplacements :: Int -> Int -> Int -> M.Map Int Int
findReachableDisplacements s1 s2 u
  | s1 > s2 = findReachableDisplacements s2 s1 u
  | otherwise =
    case ts of
      [] -> M.empty
      _ ->
        M.fromList $
          filter (\(_, s) -> (s >= s1) && (s <= s2)) $
            takeWhileWithThreshold 10 (\(_, s) -> (s >= s1) && (s <= s2)) $
              (\t -> (t, findDisplacement u t))
                <$> [minimum ts ..]
  where
    ts = catMaybes (findTime u <$> [s1, s2])

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  ((_, _), (yl, yu)) <- parseInput input
  pure $ findMaxHeight (yl, yu)

isCompatible :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
isCompatible ((xl, xu), (yl, yu)) (xv, yv) =
  (not . null $ S.intersection txs tys) || (not (null xs) && (Just (xv + 1) <= S.lookupMax tys) && (d >= xl) && (d <= xu))
  where
    xs = M.filterWithKey (\t _ -> t <= (xv + 1)) $ findReachableDisplacements xl xu xv
    d = findDisplacement xv (xv + 1)
    txs = S.fromList . M.keys $ xs
    tys = S.fromList . M.keys $ findReachableDisplacements yl yu yv

solvePart2 :: String -> Maybe String
solvePart2 input = do
  ((xl, xu), (yl, yu)) <- parseInput input
  let xvs = findVelocities xl xu
      yvs = findVelocities yl yu
  pure $ show $ length [show xv ++ "," ++ show yv | xv <- xvs, yv <- yvs, isCompatible ((xl, xu), (yl, yu)) (xv, yv)]
  where
    findVelocities l u = [((-v) - 1) .. (v + 1)]
      where
        v = abs $ findMaxVelocity l u

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  ls <- lines <$> readFile filename

  putStrLn "Part 1:"
  forM_ ls (\l -> putStrLn $ l ++ " => " ++ show (solvePart1 l))

  putStrLn ""

  putStrLn "Part 2:"
  forM_ ls (\l -> putStrLn $ l ++ " => " ++ show (solvePart2 l))
