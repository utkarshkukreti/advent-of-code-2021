module X where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn a = map T.unpack . filter (not . T.null) . T.splitOn (T.pack a) . T.pack
