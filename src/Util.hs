module Util where
-- For data manipulation utilities; things without game-related imports

import Data.List (transpose)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose

justifyLeft16 :: String -> String
justifyLeft16 str = take 16 $ str ++ repeat ' '

justifyCenter15 :: String -> String
justifyCenter15 str =
  let whitespace = replicate ((15 - length str) `div` 2) ' '
      newStr = whitespace ++ str ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "
