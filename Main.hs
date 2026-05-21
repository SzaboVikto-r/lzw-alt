module Main where

import qualified Data.ByteString as B
import Data.Bits ((.&.))
import Data.List (foldl')
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO

data Tree = Empty | Node !Char !Tree !Tree

root0 :: Tree
root0 = Node '/' Empty Empty

pathExists :: Tree -> String -> Bool
pathExists Empty        _      = False
pathExists _            []     = True
pathExists (Node _ z o) (c:cs) = pathExists (if c == '0' then z else o) cs

addLeaf :: Tree -> String -> Char -> Tree
addLeaf (Node x z o) [c] b
    | c == '0'  = Node x (Node b Empty Empty) o
    | otherwise = Node x z (Node b Empty Empty)
addLeaf (Node x z o) (c:cs) b
    | c == '0'  = Node x (addLeaf z cs b) o
    | otherwise = Node x z (addLeaf o cs b)
addLeaf _ _ _ = error "bad path"

feed :: (Tree, String) -> Char -> (Tree, String)
feed (t, p) b =
    let p' = p ++ [b]
    in  if pathExists t p'
        then (t, p')
        else (addLeaf t p' b, "")

buildLZW :: [Char] -> Tree
buildLZW = fst . foldl' feed (root0, "")

printTree :: Handle -> Tree -> Int -> IO ()
printTree _ Empty       _ = return ()
printTree h (Node c z o) d = do
    printTree h o (d + 1)
    hPutStrLn h $ replicate (3 * (d + 1)) '-' ++ [c] ++ "(" ++ show d ++ ")"
    printTree h z (d + 1)

treeDepth :: Tree -> Int
treeDepth Empty        = 0
treeDepth (Node _ z o) = 1 + max (treeDepth z) (treeDepth o)

leafDs :: Tree -> Int -> [Int]
leafDs Empty               _ = []
leafDs (Node _ Empty Empty) d = [d]
leafDs (Node _ z o)         d = leafDs z (d + 1) ++ leafDs o (d + 1)

lzwMean :: [Int] -> Double
lzwMean ds = fromIntegral (sum ds) / fromIntegral (length ds)

lzwVar :: [Int] -> Double -> Double
lzwVar ds m
    | n > 1     = sqrt (ss / fromIntegral (n - 1))
    | otherwise = sqrt ss
  where
    n  = length ds
    ss = sum [(fromIntegral d - m) ^ (2 :: Int) | d <- ds]

byteToBits :: Word8 -> [Char]
byteToBits b = [if b .&. m /= 0 then '1' else '0' | m <- [128, 64, 32, 16, 8, 4, 2, 1]]

processBytes :: Bool -> [Word8] -> [Char]
processBytes _    []           = []
processBytes _    (0x3e : rest) = processBytes True  rest
processBytes _    (0x0a : rest) = processBytes False rest
processBytes True (_ : rest)   = processBytes True  rest
processBytes _    (0x4e : rest) = processBytes False rest
processBytes _    (b    : rest) = byteToBits b ++ processBytes False rest

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inFile, "-o", outFile] -> run inFile outFile
        _ -> do
            hPutStrLn stderr "Usage: lzwtree in_file -o out_file"
            exitWith (ExitFailure (-1))

run :: FilePath -> FilePath -> IO ()
run inFile outFile = do
    bs <- B.readFile inFile
    let ws   = B.unpack bs
        rest = drop 1 $ dropWhile (/= 0x0a) ws
        bits = processBytes False rest
        tree = buildLZW bits
        ds   = leafDs tree 0
        m    = lzwMean ds
        v    = lzwVar ds m
    withFile outFile WriteMode $ \h -> do
        printTree h tree 0
        hPutStrLn h $ "depth = " ++ show (treeDepth tree - 1)
        hPutStrLn h $ "mean = "  ++ show m
        hPutStrLn h $ "var = "   ++ show v
