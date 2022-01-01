import Control.Monad (forM_, join)
import Data.Bifunctor (Bifunctor (first))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

splitAtStrict :: Int -> [a] -> Maybe ([a], [a])
splitAtStrict n xs =
  if length as == n then Just (as, bs) else Nothing
  where
    (as, bs) = splitAt n xs

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs =
  if length as == n
    then as : chunks n bs
    else []
  where
    (as, bs) = splitAt n xs

data Bit = Z | O deriving (Eq)

instance Show Bit where
  show Z = "0"
  show O = "1"

type Binary = [Bit]

bit2Int :: Bit -> Int
bit2Int Z = 0
bit2Int O = 1

binary2Int :: Binary -> Int
binary2Int = foldl (\a x -> a + a + bit2Int x) 0

hexadecimalToBinary :: String -> Maybe Binary
hexadecimalToBinary = fmap join . traverse go
  where
    -- 0-3
    go '0' = Just [Z, Z, Z, Z]
    go '1' = Just [Z, Z, Z, O]
    go '2' = Just [Z, Z, O, Z]
    go '3' = Just [Z, Z, O, O]
    -- 4-7
    go '4' = Just [Z, O, Z, Z]
    go '5' = Just [Z, O, Z, O]
    go '6' = Just [Z, O, O, Z]
    go '7' = Just [Z, O, O, O]
    -- 8-11
    go '8' = Just [O, Z, Z, Z]
    go '9' = Just [O, Z, Z, O]
    go 'A' = Just [O, Z, O, Z]
    go 'B' = Just [O, Z, O, O]
    -- 12-15
    go 'C' = Just [O, O, Z, Z]
    go 'D' = Just [O, O, Z, O]
    go 'E' = Just [O, O, O, Z]
    go 'F' = Just [O, O, O, O]
    -- otherwise,
    go _ = Nothing

newtype LiteralValue = LiteralValue {value :: Int} deriving (Show, Eq)

newtype Operator = Operator {subpackets :: (Packet, [Packet])} deriving (Show, Eq)

data Packet
  = LiteralValuePacket Int Int LiteralValue
  | OperatorPacket Int Int Operator
  deriving (Show, Eq)

version :: Packet -> Int
version (LiteralValuePacket v _ _) = v
version (OperatorPacket v _ _) = v

versions :: Packet -> [Int]
versions (LiteralValuePacket v _ _) = [v]
versions (OperatorPacket v _ (Operator (sp, sps))) = v : (versions sp ++ join (map versions sps))

lengthOfPacket :: Packet -> Int
lengthOfPacket (LiteralValuePacket _ l _) = l
lengthOfPacket (OperatorPacket _ l _) = l

fromBinaryToPacket :: Binary -> Maybe (Packet, Binary)
fromBinaryToPacket xs = do
  let versionBits = 3
      packetTypeBits = 3
  (v, xs) <- first binary2Int <$> splitAtStrict versionBits xs
  (pt, xs) <- first binary2Int <$> splitAtStrict packetTypeBits xs
  if pt == 4
    then do
      (l, val, xs) <- pure $ go (versionBits + packetTypeBits) 0 xs
      pure (LiteralValuePacket v l (LiteralValue val), xs)
    else do
      ([typeId], xs) <- splitAtStrict 1 xs
      case typeId of
        Z -> do
          let lsubpacketsBits = 15
          (lsubpackets, xs) <- first binary2Int <$> splitAtStrict lsubpacketsBits xs
          (sp : sps, l, xs) <- pure $ go' 0 lsubpackets xs
          pure (OperatorPacket v (l + lsubpacketsBits + 1 + versionBits + packetTypeBits) (Operator (sp, sps)), xs)
        O -> do
          let nsubpacketsBits = 11
          (nsubpackets, xs) <- first binary2Int <$> splitAtStrict nsubpacketsBits xs
          (sp : sps, l, xs) <- pure $ go'' 0 nsubpackets xs
          pure (OperatorPacket v (l + nsubpacketsBits + 1 + versionBits + packetTypeBits) (Operator (sp, sps)), xs)
  where
    go l acc (Z : a : b : c : d : rest) = (l + 5, (acc * 16) + binary2Int [a, b, c, d], rest)
    go l acc (O : a : b : c : d : rest) = go (l + 5) ((acc * 16) + binary2Int [a, b, c, d]) rest
    go l acc rest = (l, acc, rest)

    go' l lub xs =
      case fromBinaryToPacket xs of
        Nothing -> ([], l, xs)
        Just (packet, rest) -> let (packets, l', rest') = go' (l + lengthOfPacket packet) lub rest in (packet : packets, l', rest')

    go'' l n xs
      | n <= 0 = ([], l, xs)
      | otherwise =
        case fromBinaryToPacket xs of
          Nothing -> ([], l, xs)
          Just (packet, rest) -> let (packets, l', rest') = go'' (l + lengthOfPacket packet) (n - 1) rest in (packet : packets, l', rest')

fromHexadecimalToPacket :: String -> Maybe (Packet, Binary)
fromHexadecimalToPacket hxs = do
  xs <- hexadecimalToBinary hxs
  fromBinaryToPacket xs

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  (packet, _) <- fromHexadecimalToPacket input
  pure $ sum $ versions packet

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- lines <$> readFile filename

  putStrLn "Part 1:"
  forM_ content (\input -> putStrLn $ input ++ ": " ++ show (solvePart1 input))