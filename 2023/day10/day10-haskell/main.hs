import System.IO (hFlush, stdout)
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Array ((!))

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

type Grid = A.Array (Int, Int) Char

startingNode :: Grid -> (Int, Int)
startingNode = fst . head . filter ((==) 'S' . snd) . A.assocs

parseGridFromString :: String -> Grid
parseGridFromString s = A.array ((0, 0), (height-1, width-1)) [
        ((i, j), chr)
        | (i, line) <- zip [0..] ls
        , (j, chr) <- zip [0..] line
    ]
    where
        ls = lines s
        width = length $ head ls
        height = length ls

parseGridFromFile :: String -> IO Grid
parseGridFromFile = fmap parseGridFromString . readFile

north :: (Int, Int) -> (Int, Int)
north (i, j) = (i-1, j)

south :: (Int, Int) -> (Int, Int)
south (i, j) = (i+1, j)

east :: (Int, Int) -> (Int, Int)
east (i, j) = (i, j+1)

west :: (Int, Int) -> (Int, Int)
west  (i, j) = (i, j-1)

solvePart1ForGrid :: Grid -> Int
solvePart1ForGrid grid = (+(-1)) . length $ bfs neighbors sNode
    where
        sNode = startingNode grid
        inBounds = filter (A.inRange $ A.bounds grid)
        nsew ix  = inBounds [north ix, south ix, east ix, west ix]
        neighbors ix =
            if ix == sNode then
                filter (elem ix . neighbors) $ nsew ix
            else
                case grid ! ix of
                    'L' -> inBounds [north ix, east ix ]
                    'J' -> inBounds [north ix, west ix ]
                    '7' -> inBounds [south ix, west ix ]
                    'F' -> inBounds [south ix, east ix ]
                    '|' -> inBounds [north ix, south ix]
                    '-' -> inBounds [east ix , west ix ]
                    _   -> []

solvePart1ForFile :: String -> IO Int
solvePart1ForFile = fmap solvePart1ForGrid . parseGridFromFile

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    grid     <- parseGridFromFile filePath
    let part1Ans = solvePart1ForGrid grid
    putStrLn $ "Part 1: " ++ show part1Ans
