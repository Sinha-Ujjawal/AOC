import Control.Monad (join)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

lookup' :: (Show a, Ord a) => a -> M.Map a b -> b
lookup' a m =
  case M.lookup a m of
    Just b -> b
    Nothing -> error $ "Key: " ++ show a ++ " not found!"

data PriorityQueue a b = PriorityQueue {key2priority :: M.Map a b, priority2keys :: M.Map b (S.Set a)} deriving (Show, Eq)

emptyPQ :: (Ord a, Ord b) => PriorityQueue a b
emptyPQ = PriorityQueue M.empty M.empty

fromList :: (Ord a, Ord b) => [(b, a)] -> PriorityQueue a b
fromList = foldl (\pq (b, a) -> setPriority a b pq) emptyPQ

member :: Ord a => a -> PriorityQueue a b -> Bool
member a (PriorityQueue m _) = M.member a m

setPriority :: (Ord a, Ord b) => a -> b -> PriorityQueue a b -> PriorityQueue a b
setPriority a b (PriorityQueue m m') =
  case M.lookup a m of
    Just b' ->
      let m'' = M.insert a b m
          m''' = M.update (\s -> let s' = S.delete a s in if null s' then Nothing else Just s') b' m'
       in PriorityQueue m'' (M.alter alter' b m''')
    Nothing -> PriorityQueue (M.insert a b m) (M.alter alter' b m')
  where
    alter' (Just s) = Just $ S.insert a s
    alter' Nothing = Just $ S.fromList [a]

lookupPriority :: (Ord a, Ord b) => a -> PriorityQueue a b -> Maybe b
lookupPriority a (PriorityQueue m _) = M.lookup a m

lookupMax :: (Ord a, Ord b) => PriorityQueue a b -> Maybe (b, S.Set a)
lookupMax (PriorityQueue _ m) = M.lookupMax m

lookupMin :: (Ord a, Ord b) => PriorityQueue a b -> Maybe (b, S.Set a)
lookupMin (PriorityQueue _ m) = M.lookupMin m

deleteMax :: (Ord a, Ord b) => PriorityQueue a b -> (Maybe (b, a), PriorityQueue a b)
deleteMax pq@(PriorityQueue m m') = do
  case lookupMax pq of
    Just (b, s) ->
      let (a, s') = S.deleteFindMin s
       in (Just (b, a), PriorityQueue (M.delete a m) (if null s' then M.delete b m' else M.insert b s' m'))
    Nothing -> (Nothing, pq)

deleteMin :: (Ord a, Ord b) => PriorityQueue a b -> (Maybe (b, a), PriorityQueue a b)
deleteMin pq@(PriorityQueue m m') = do
  case lookupMin pq of
    Just (b, s) ->
      let (a, s') = S.deleteFindMin s
       in (Just (b, a), PriorityQueue (M.delete a m) (if null s' then M.delete b m' else M.insert b s' m'))
    Nothing -> (Nothing, pq)

--------------------------------------------------------------------------------------------------------------------------------

bestPath :: (Ord a, Show a, Ord b, Num b) => (a -> a -> b) -> (a -> S.Set a) -> a -> a -> [(a, b)]
bestPath weightFn neighborFn start end = go visited pq dist parent
  where
    visited = S.empty
    pq = fromList [(0, start)]
    dist = M.fromList [(start, 0)]
    parent = M.empty

    go visited pq dist parent =
      case deleteMin pq of
        (Just (du, u), pq') ->
          if u == end
            then buildPath parent dist
            else
              let visited' = S.insert u visited
                  (pq'', dist', parent') = foldl folder (pq', dist, parent) $ neighborFn u
               in go visited' pq'' dist' parent'
          where
            folder (pq, dist, parent) v =
              let dv' = du + weightFn u v
               in case M.lookup v dist of
                    Just dv ->
                      if dv' < dv
                        then (setPriority v dv' pq, M.insert v dv' dist, M.insert v u parent)
                        else (pq, dist, parent)
                    Nothing ->
                      (setPriority v dv' pq, M.insert v dv' dist, M.insert v u parent)
        (Nothing, _) -> []

    buildPath parent dist = go [] end
      where
        go acc node
          | node == start = (start, lookup' start dist) : acc
          | otherwise = go ((node, lookup' node dist) : acc) (lookup' node parent)

-----------------------------------------------------------------------------------------------

type Grid a = M.Map (Int, Int) a

shape :: Grid a -> Maybe (Int, Int)
shape = fmap ((\(x, y) -> (x + 1, y + 1)) . fst) . M.lookupMax

parseInput :: String -> Maybe (Grid Int)
parseInput = fmap (M.fromList . join) . traverse parseRow . zip [0 ..] . lines
  where
    parseRow (i, row) = traverse parseCell $ zip [0 ..] row
      where
        parseCell (j, val) = do
          val <- readMaybe [val]
          pure ((i, j), val)

showPath :: Int -> Int -> [(Int, Int)] -> String
showPath rows cols path = unlines [[if S.member (i, j) path' then '#' else ' ' | j <- [0 .. cols]] | i <- [0 .. rows]]
  where
    path' = S.fromList path

int64Max :: Int
int64Max = 2 ^ 63 - 1

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  grid <- parseInput input
  (rows, cols) <- shape grid
  let neighborFn (i, j) =
        S.fromList $
          filter
            (\(a, b) -> a >= 0 && a < rows && b >= 0 && b < cols)
            [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]
      wtFn _ (0, 0) = 0
      wtFn _ (i, j) = fromMaybe int64Max $ M.lookup (i, j) grid
      start = (0, 0)
      end = (rows - 1, cols - 1)
  pure . snd . last $ bestPath wtFn neighborFn start end

solvePart2 :: String -> Maybe Int
solvePart2 input = do
  grid <- parseInput input
  (rows, cols) <- shape grid
  let (rows', cols') = (rows * 5, cols * 5)
  let neighborFn (i, j) =
        S.fromList $
          filter
            (\(a, b) -> a >= 0 && a < rows' && b >= 0 && b < cols')
            [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]

      wtFn _ (0, 0) = 0
      wtFn _ (i, j) = maybe int64Max (\w -> 1 + rem (w + offset - 1) 9) $ M.lookup (i', j') grid
        where
          (i', j') = (rem i cols, rem j rows)
          offset = div i cols + div j rows

      start = (0, 0)
      end = (rows' - 1, cols' - 1)
  pure . snd . last $ bestPath wtFn neighborFn start end

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans