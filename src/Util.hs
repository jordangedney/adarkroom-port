module Util
where

import           Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose

justifyLeft16 str = take 16 $ str ++ repeat ' '

justifyCenter15 str =
  let whitespace = replicate ((15 - length str) `div` 2) ' '
      newStr = whitespace ++ str ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "

addEvent :: String -> [(String, Int)] -> [(String, Int)]
addEvent e es = (e, 0) : es
