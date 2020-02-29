module Util
where

import           Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose

justifyCenter15 str =
  let whitespace = replicate ((15 - length str) `div` 2) ' '
      newStr = whitespace ++ str ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "
