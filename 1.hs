import Data.Char
import Data.List

parse :: String -> ([Int], [Int])
parse = unzip . map (\s -> (read $ takeWhile isNumber s, read $ takeWhile isNumber . dropWhile isSpace . dropWhile isNumber $ s)) . lines

solve1a :: ([Int], [Int]) -> Int
solve1a (xs, ys) = sum . map abs $ zipWith (-) (sort xs) (sort ys)

solve1b :: ([Int], [Int]) -> Int
solve1b (xs, ys) = foldl (\acc x -> flip (+) acc $ flip (*) x $ length . filter (== x) $ ys) 0 xs

main = do
  contents1a <- readFile "1a.txt"
  print (solve1a . parse $ contents1a)

  contents1b <- readFile "1b.txt"
  print (solve1b . parse $ contents1b)
