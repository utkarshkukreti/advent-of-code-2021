{-# LANGUAGE TupleSections #-}

import qualified Data.IntMap as M
import X (splitOn)

main :: IO ()
main = do
  xs <- map read . splitOn "," <$> getContents
  print $ run xs !! 80
  print $ run xs !! 256

run :: [Int] -> [Int]
run =
  map (sum . M.elems)
    . iterate (M.fromListWith (+) . concatMap go . M.toList)
    . M.fromListWith (+)
    . map (,1)
  where
    go (0, v) = [(6, v), (8, v)]
    go (k, v) = [(k - 1, v)]
