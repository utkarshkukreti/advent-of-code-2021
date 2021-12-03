import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)

main :: IO ()
main = do
  xs <- lines <$> getContents
  print $ p1 xs
  print $ p2 xs

p1 :: [String] -> Int
p1 xs = go mostFrequent * go leastFrequent
  where
    go f = toInt . map f $ transpose xs

p2 :: [String] -> Int
p2 xs = go mostFrequent * go leastFrequent
  where
    go f = toInt . head $ foldl go2 xs [0 .. length (head xs) - 1]
      where
        go2 xs i = do
          let x = f $ map (!! i) xs
          filter ((== x) . (!! i)) xs

mostFrequent :: String -> Char
mostFrequent = head . maximumBy (comparing length) . group . sort

leastFrequent :: String -> Char
leastFrequent = head . minimumBy (comparing length) . group . sort

toInt :: String -> Int
toInt = foldl (\acc x -> acc * 2 + read [x]) 0
