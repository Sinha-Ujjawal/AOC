import System.IO (hFlush, stdout)
import qualified Data.Array as A
import Data.Array ((!))

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

type Image = A.Array (Int, Int) Char

parseImageFromString :: String -> Image
parseImageFromString txt = A.array ((1, 1), (height, width)) [
        ((row, col), char)
        | (row, line) <- zip [1..] ls
        , (col, char) <- zip [1..] line
    ]
    where
        ls = lines txt
        height = length ls
        width = length $ head ls

parseImageFromFile :: String -> IO Image
parseImageFromFile = fmap parseImageFromString . readFile

findGalaxiesFromImage :: Image -> [(Int, Int)]
findGalaxiesFromImage = fmap fst . filter ((== '#') . snd) . A.assocs

findExpanedGalaxiesFromImage :: Int -> Int -> Image -> [(Int, Int)]
findExpanedGalaxiesFromImage colOff rowOff image = galaxiesExpandedByRows
  where
    gs = findGalaxiesFromImage image
    colsOfGalaxies = fmap snd gs
    rowsOfGalaxies = fmap fst gs

    ((rowl, coll), (rowh, colh)) = A.bounds image
    colsWithoutGalaxies = reverse (filter (not . (`elem` colsOfGalaxies)) [coll..colh])
    rowsWithoutGalaxies = reverse (filter (not . (`elem` rowsOfGalaxies)) [rowl..rowh])

    galaxiesExpandedByColumns = foldl (\gs col -> fmap (\g@(row', col') -> if col' > col then (row', col'+colOff) else g) gs) gs colsWithoutGalaxies
    galaxiesExpandedByRows    = foldl (\gs row -> fmap (\g@(row', col') -> if row' > row then (row'+rowOff, col') else g) gs) galaxiesExpandedByColumns rowsWithoutGalaxies

findAllPairDistanceOfGalaxies :: [(Int, Int)] -> [((Int, Int), (Int, Int), Int)]
findAllPairDistanceOfGalaxies gs = [
        (start, end, abs (row'-row) + abs(col'-col))
        | start@(row, col)   <- gs
        , end@(row', col')   <- gs
        , start < end
    ]

sumAllPairDistancesOfGalaxies :: Int -> Image -> Int
sumAllPairDistancesOfGalaxies expandBy =
    sum
    . fmap (\(_, _, dist) -> dist)
    . findAllPairDistanceOfGalaxies
    . findExpanedGalaxiesFromImage (expandBy-1) (expandBy-1)

solvePart1ForImage :: Image -> Int
solvePart1ForImage = sumAllPairDistancesOfGalaxies 2

solvePart1ForFile :: String -> IO Int
solvePart1ForFile = fmap solvePart1ForImage . parseImageFromFile

solvePart2ForImage :: Image -> Int
solvePart2ForImage = sumAllPairDistancesOfGalaxies 1000000

solvePart2ForFile :: String -> IO Int
solvePart2ForFile = fmap solvePart2ForImage . parseImageFromFile

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    image <- parseImageFromFile filePath
    let part1Ans = solvePart1ForImage image
        part2Ans = solvePart2ForImage image
    putStrLn $ "Part 1: " ++ show part1Ans
    putStrLn $ "Part 2: " ++ show part2Ans
