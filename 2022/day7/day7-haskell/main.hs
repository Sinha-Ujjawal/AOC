{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

newtype Path = Path T.Text deriving (Ord, Show, Eq)

instance Semigroup Path where
  p1@(Path _) <> Path "" = p1
  Path "" <> p2@(Path _) = p2
  Path s1 <> Path s2 = Path (T.concat [s1, "/", s2])

instance Monoid Path where
  mempty = Path ""

sanitizePath :: Path -> Path
sanitizePath (Path path) = Path (sanitizePath' path)
  where
    sanitizePath' :: T.Text -> T.Text
    sanitizePath' = T.intercalate "/" . go . T.splitOn "/"

    go :: [T.Text] -> [T.Text]
    go (x : ".." : rest) = go rest
    go ("" : rest) = go rest
    go (x : rest) = x : go rest
    go [] = []

type DirInfo = T.Text

type FileInfo = (T.Text, Int)

type DirectoryItem = Either DirInfo FileInfo

type Directory = M.Map Path (S.Set DirectoryItem)

collectDirInfo :: S.Set DirectoryItem -> S.Set DirInfo
collectDirInfo = S.fromList . collectDirInfo' . S.toList
  where
    collectDirInfo' :: [DirectoryItem] -> [DirInfo]
    collectDirInfo' [] = []
    collectDirInfo' (Left dir : rest) = dir : collectDirInfo' rest
    collectDirInfo' (Right _ : rest) = collectDirInfo' rest

subDirInfo :: Directory -> S.Set DirInfo
subDirInfo = maybe S.empty collectDirInfo . M.lookup mempty

subDirs :: Directory -> M.Map DirInfo Directory
subDirs dir = M.fromSet mkSubDir (subDirInfo dir)
  where
    mkSubDir :: DirInfo -> Directory
    mkSubDir subDirInfo = M.filterWithKey (const . pathContains subDirInfo) dir

    pathContains :: DirInfo -> Path -> Bool
    pathContains dirName (Path path) = T.isPrefixOf dirName path

dirItemsSize :: S.Set DirectoryItem -> Int
dirItemsSize = S.foldr (\dirItem accum -> accum + dirItemSize dirItem) 0
  where
    dirItemSize :: DirectoryItem -> Int
    dirItemSize (Right (_, s)) = s
    dirItemSize _ = 0

directorySize :: Directory -> Int
directorySize = M.foldr (\dirItems accum -> accum + dirItemsSize dirItems) 0

data Command = CD !Path | LS deriving (Show)

parseCommand :: T.Text -> Maybe Command
parseCommand "$ ls" = Just LS
parseCommand (T.stripPrefix "$ cd " -> Just path) = Just $ CD (Path path)
parseCommand _ = Nothing

parseDirInfo :: T.Text -> Maybe DirInfo
parseDirInfo = T.stripPrefix "dir "

parseFileInfo :: T.Text -> Maybe FileInfo
parseFileInfo (T.splitOn " " -> [fileSizeStr, fileName]) = do
  fileSize <- readMaybe (T.unpack fileSizeStr)
  return (fileName, fileSize)
parseFileInfo _ = Nothing

parseDirItem :: T.Text -> Maybe DirectoryItem
parseDirItem (parseDirInfo -> Just dir) = Just (Left dir)
parseDirItem (parseFileInfo -> Just file) = Just (Right file)
parseDirItem _ = Nothing

parse :: T.Text -> Maybe Directory
parse = parsing mempty M.empty . T.lines . T.strip
  where
    parsing :: Path -> Directory -> [T.Text] -> Maybe Directory
    parsing _ d [] = Just d
    parsing p d (line : rest) =
      case parseCommand line of
        Nothing -> Nothing
        Just (CD p') -> parsing (sanitizePath (p <> p')) d rest
        Just LS -> do
          (dirItems, rest') <- parseDirItems S.empty rest
          parsing p (M.insert p dirItems d) rest'

    parseDirItems :: S.Set DirectoryItem -> [T.Text] -> Maybe (S.Set DirectoryItem, [T.Text])
    parseDirItems dirItems [] = Just (dirItems, [])
    parseDirItems dirItems ls@(line : rest) =
      case parseDirItem line of
        Nothing -> Just (dirItems, ls)
        Just dirItem -> parseDirItems (S.insert dirItem dirItems) rest

allDirectorySizes :: Directory -> [(Path, Int)]
allDirectorySizes dir = map accumSize lst
  where
    lst = M.toList $ M.map dirItemsSize dir

    accumSize (p, sz) = (p, sz + sum (filterSizes p))

    filterSizes (Path p) =
      map snd $
        filter ((\(Path q) -> T.isPrefixOf p q && p /= q) . fst) lst

solvePart1 :: Directory -> Int
solvePart1 = sum . filter (<= 100000) . map snd . allDirectorySizes

parseDirectoryFromFile :: FilePath -> IO Directory
parseDirectoryFromFile filePath = do
  rootMaybe <- parse . T.pack <$> readFile filePath
  case rootMaybe of
    Nothing -> error $ "Unable to parse " ++ filePath
    Just root -> return root

solvePart2 :: Directory -> Int
solvePart2 dir = head . dropWhile (< need) . L.sort $ map snd sizes
  where
    sizes = allDirectorySizes dir
    need = 30000000 - 70000000 + snd (head sizes)

sample :: IO Directory
sample = parseDirectoryFromFile "../sample.txt"

input :: IO Directory
input = parseDirectoryFromFile "../input.txt"

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  root <- parseDirectoryFromFile filePath
  let part1Ans = solvePart1 root
  putStrLn $ "Part 1: " ++ show part1Ans
  let part2Ans = solvePart2 root
  putStrLn $ "Part 2: " ++ show part2Ans
