{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

data PasswordPolicy = PasswordPolicy Int Int Char deriving (Show, Eq)

type Password = (PasswordPolicy, String)

nth :: Int -> [a] -> Maybe a
nth 1 (x : _) = Just x
nth i (_ : xs) | i > 1 = nth (i - 1) xs
nth i _ | i < 1 = Nothing

readTextMaybe :: Read a => T.Text -> Maybe a
readTextMaybe = readMaybe . T.unpack

parseLineMaybe :: String -> Maybe Password
parseLineMaybe = toPasswordPolicy . parsePasswordPolicy . T.words . T.pack
  where
    parsePasswordPolicy [bounds, char, password] = parsePasswordPolicy $ T.splitOn "-" bounds ++ [char, password]
    parsePasswordPolicy [lb, ub, T.unpack -> [chr, ':'], password] = Just (readTextMaybe lb, readTextMaybe ub, chr, password)
    parsePasswordPolicy _ = Nothing

    toPasswordPolicy (Just (Just lb, Just ub, chr, password)) = Just (PasswordPolicy lb ub chr, T.unpack password)
    toPasswordPolicy _ = Nothing

parseLines :: [String] -> [Password]
parseLines = mapMaybe parseLineMaybe

solver :: (Password -> Bool) -> [String] -> Int
solver validator =
  length
    . filter id
    . map validator
    . parseLines

validPassword1 :: Password -> Bool
validPassword1 (PasswordPolicy lb ub chr, password) =
  let l = length $ filter (== chr) password
   in (l >= lb) && (l <= ub)

solvePart1 :: [String] -> Int
solvePart1 = solver validPassword1

validPassword2 :: Password -> Bool
validPassword2 (PasswordPolicy i j chr, password) =
  let atI = Just chr == nth i password
      atJ = Just chr == nth j password
   in atI /= atJ

solvePart2 :: [String] -> Int
solvePart2 = solver validPassword2

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- lines <$> readFile filename

  let part1Ans = solvePart1 fileLines
      part2Ans = solvePart2 fileLines

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans