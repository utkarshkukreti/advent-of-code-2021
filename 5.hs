import Data.Char (isDigit)
import Data.List (group, sort)
import X (splitOn)

main :: IO ()
main = do
  xs <- map ((\[[a, b], [c, d]] -> (a, b, c, d)) . (map (map read . splitOn ",") . splitOn " -> ")) . lines <$> getContents
  let xs' = filter (\(x1, y1, x2, y2) -> x1 == x2 || y1 == y2) xs
  print $ count xs'
  print $ count xs

count :: [(Int, Int, Int, Int)] -> Int
count = length . filter (>= 2) . map length . group . sort . concatMap go
  where
    go (x1, y1, x2, y2) = do
      let dx = signum $ x2 - x1
      let dy = signum $ y2 - y1
      zip [x1, x1 + dx .. x2] [y1, y1 + dy .. y2]
