import System.IO (hFlush, stdout)
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Array ((!), (//))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

bfs :: Ord a => (a -> [a]) -> a -> [S.Set a]
bfs neighbors start = go startingFrontier startingFrontier
  where
    startingFrontier = S.singleton start

    go seenSet frontier = if S.size frontier == 0 then [] else frontier : go seenSet' frontier'
        where
            seenSet'  = S.union seenSet frontier
            frontier' = S.fromList [v | u <- S.toList frontier, v <- neighbors u, not (S.member v seenSet')]

cycles :: (Eq a, Ord a) => (a -> [a]) -> a -> [[a]]
cycles neighbors start = (start:) <$> go (S.singleton start) start
    where
        go seenSet u = do
            v <- neighbors u
            if v == start then
                [[v]]
            else if S.member v seenSet then
                []
            else
                [v : ts | ts <- go (S.insert v seenSet) v, ts /= []]

type Grid = A.Array (Int, Int) Char
type Input = (Grid, (Int, Int))

north :: (Int, Int) -> (Int, Int)
north (i, j) = (i-1, j)

south :: (Int, Int) -> (Int, Int)
south (i, j) = (i+1, j)

east :: (Int, Int) -> (Int, Int)
east (i, j) = (i, j+1)

west :: (Int, Int) -> (Int, Int)
west  (i, j) = (i, j-1)

easts :: Grid -> (Int, Int) -> [(Int, Int)]
easts grid ix = takeWhile (inBounds grid) $ iterate east ix

inBounds :: (A.Ix ix) => A.Array ix a -> ix -> Bool
inBounds = A.inRange . A.bounds

nsew :: Grid -> (Int, Int) -> [(Int, Int)]
nsew grid ix = filter (inBounds grid) [north ix, south ix, east ix, west ix]

startNodeChar :: Input -> Char
startNodeChar input@(grid, sNode) =
    case (\ix -> inBounds grid ix && elem sNode (neighbors input ix)) <$> [north sNode, south sNode, east sNode, west sNode] of
        [True, True, _   , _   ] -> '|'
        [True, _   , True, _   ] -> 'L'
        [True, _   , _   , True] -> 'J'
        [_   , True, True, _   ] -> 'F'
        [_   , True, _   , True] -> '7'
        [_   , _   , True, True] -> '-'
        _                        -> error "Unreachable!"

neighbors :: Input -> (Int, Int) -> [(Int, Int)]
neighbors input@(grid, sNode) ix =
    case grid ! ix of
        'L' -> filter (inBounds grid) [north ix, east ix ]
        'J' -> filter (inBounds grid) [north ix, west ix ]
        '7' -> filter (inBounds grid) [south ix, west ix ]
        'F' -> filter (inBounds grid) [south ix, east ix ]
        '|' -> filter (inBounds grid) [north ix, south ix]
        '-' -> filter (inBounds grid) [east ix , west ix ]
        _   -> []

parseInputFromString :: String -> Input
parseInputFromString s = (grid', sNode)
    where
        ls = lines s
        height = length ls
        width = length $ head ls
        assocs = [
                ((i, j), chr)
                | (i, line) <- zip [0..] ls
                , (j, chr) <- zip [0..] line
            ]
        grid = A.array ((0, 0), (height-1, width-1)) assocs
        sNode = fst . head $ filter ((==) 'S' . snd) assocs
        sNodeChar = startNodeChar (grid, sNode)
        grid' = grid // [(sNode, sNodeChar)]

parseInputFromFile :: String -> IO Input
parseInputFromFile = fmap parseInputFromString . readFile

solvePart1ForInput :: Input -> Int
solvePart1ForInput input@(grid, sNode) = (+(-1)) . length $ bfs (neighbors input) sNode

solvePart1ForFile :: String -> IO Int
solvePart1ForFile = fmap solvePart1ForInput . parseInputFromFile

solvePart2ForInput :: Input -> Int
solvePart2ForInput input@(grid, sNode) = S.size $ S.filter insideLoop allPossibleTiles
    where
        polygon = maximumBy (compare `on` length) $ cycles (neighbors input) sNode
        polygonSet = S.fromList polygon
        
        (low, high) = A.bounds grid
        is = fmap fst polygon
        js = fmap snd polygon
        
        possibleMin = if S.size polygonSet > 0 then (minimum is, minimum js) else low
        possibleMax = if S.size polygonSet > 0 then (maximum is, maximum js) else high
        allPossibleTiles = S.fromList [ix | ix <- A.range (possibleMin, possibleMax), not (S.member ix polygonSet)]
        
        isIntersection ix =  S.member ix polygonSet && S.member (grid ! ix) (S.fromList ['|', '7', 'F'])

        insideLoop =
            odd
            . length
            . filter isIntersection
            . easts grid

solvePart2ForFile :: String -> IO Int
solvePart2ForFile = fmap solvePart2ForInput . parseInputFromFile

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    input    <- parseInputFromFile filePath
    let part1Ans = solvePart1ForInput input
        part2Ans = solvePart2ForInput input
    putStrLn $ "Part 1: " ++ show part1Ans
    putStrLn $ "Part 2: " ++ show part2Ans
