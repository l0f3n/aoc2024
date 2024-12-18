import Data.Char
import Data.List

parse :: String -> [[Int]]
parse = map (map read . filter (all isNumber) . groupBy (\x y -> isNumber x && isNumber y)) . lines

solveA :: [[Int]] -> Int
solveA = length . filter isSafe

solveB :: [[Int]] -> Int
solveB = length . filter (any isSafe) . map oneRemovedVariants

isSafe :: [Int] -> Bool
isSafe xs = isGood . zipWith (-) xs $ drop 1 xs
  where
    isGood xs = isIncOrDec xs && isGradual xs
    isIncOrDec xs = all (< 0) xs || all (> 0) xs
    isGradual = all $ (<= 3) . abs

oneRemovedVariants :: [a] -> [[a]]
oneRemovedVariants xs = [take n xs ++ drop (n + 1) xs | n <- [0 .. length xs - 1]]

main = do
  contents <- readFile "2.txt"

  print (solveA . parse $ contents)
  print (solveB . parse $ contents)
