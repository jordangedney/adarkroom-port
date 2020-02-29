module Util
        ( count
        , interleave
        )
where

import           Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose
