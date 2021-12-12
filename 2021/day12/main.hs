{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isUpper)
import Data.Foldable (foldlM)
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

data Node = Start | End | SmallCave String | BigCave String deriving (Show, Eq, Ord)

type Graph = M.Map Node (S.Set Node)

type Path = [Node]

toNode :: String -> Maybe Node
toNode "start" = Just Start
toNode "end" = Just End
toNode xs@(x : _) = Just $ if isUpper x then BigCave xs else SmallCave xs
toNode _ = Nothing

toGraph :: String -> Maybe Graph
toGraph graphSpec = do
  let edges = map (map T.unpack . T.splitOn "-" . T.pack) $ lines graphSpec
  foldlM folder M.empty edges
  where
    folder m [fromStr, toStr] = do
      from <- toNode fromStr
      to <- toNode toStr
      let m' = M.insertWith S.union from (S.fromList [to]) m
          m'' = M.insertWith S.union to (S.fromList [from]) m'
      pure m''

neighbors :: Node -> Graph -> S.Set Node
neighbors node graph = fromMaybe S.empty $ M.lookup node graph

distinctPathsThatVisitsSmallCavesOnce :: Graph -> S.Set Path
distinctPathsThatVisitsSmallCavesOnce graph = dfs Start S.empty [] S.empty
  where
    isNotBigCave (BigCave _) = False
    isNotBigCave _ = True

    dfs End _ acc paths = S.insert (reverse (End : acc)) paths
    dfs node visited acc paths =
      if S.member node visited
        then paths
        else
          let visited' = if isNotBigCave node then S.insert node visited else visited
              acc' = node : acc
           in foldl (\paths' neighbor -> dfs neighbor visited' acc' paths') paths $ neighbors node graph

distinctPathsThatVisitsASmallCaveAtMostTwice :: Graph -> S.Set Path
distinctPathsThatVisitsASmallCaveAtMostTwice graph = dfs Start S.empty Nothing 0 [] S.empty
  where
    isNotBigCave (BigCave _) = False
    isNotBigCave _ = True

    isSmallCave (SmallCave _) = True
    isSmallCave _ = False

    dfs End _ _ _ acc paths = S.insert (reverse (End : acc)) paths
    dfs node visited nodeConsideredToBeVisitedAtMostTwice visitCount acc paths
      | S.member node visited =
        if isSmallCave node && visitCount == 1 && Just node == nodeConsideredToBeVisitedAtMostTwice
          then
            let visited' = S.insert node visited
                acc' = node : acc
             in foldl (\paths' neighbor -> dfs neighbor visited' nodeConsideredToBeVisitedAtMostTwice 2 acc' paths') paths $ neighbors node graph
          else paths
      | not (S.member node visited) && isSmallCave node && visitCount == 0 =
        let visited' = S.insert node visited
            acc' = node : acc
            paths' = foldl (\paths' neighbor -> dfs neighbor visited' (Just node) 1 acc' paths') paths $ neighbors node graph
            paths'' = foldl (\paths' neighbor -> dfs neighbor visited' Nothing 0 acc' paths') paths' $ neighbors node graph
         in paths''
      | not (S.member node visited) =
        let visited' = if isNotBigCave node then S.insert node visited else visited
            acc' = node : acc
         in foldl (\paths' neighbor -> dfs neighbor visited' nodeConsideredToBeVisitedAtMostTwice visitCount acc' paths') paths $ neighbors node graph
      | otherwise = paths

solvePart1 :: String -> Maybe Int
solvePart1 graphSpec = do
  graph <- toGraph graphSpec
  pure $ length $ distinctPathsThatVisitsSmallCavesOnce graph

solvePart2 :: String -> Maybe Int
solvePart2 graphSpec = do
  graph <- toGraph graphSpec
  pure $ length $ distinctPathsThatVisitsASmallCaveAtMostTwice graph

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans