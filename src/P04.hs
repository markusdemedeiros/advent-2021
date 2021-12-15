module P04 where 

import Data.List (transpose, partition)
import Data.List.Split (splitOn)

         
-- map ((> length i) . (*2) . length . filter (== '1')) . transpose $ i


type Board = [[Maybe Int]]




parse :: IO ([Int], [Board])
parse = do dat <- readFile "data/04/data.txt" >>= return. lines
           return ( map read. splitOn ",". (!!0). head. splitOn [""] $ dat
                  , map(map(map(Just. read). filter (not. null). splitOn " ")). tail. splitOn [""] $ dat )

solve :: IO ()
solve = do
    inp <- parse
    return ()
    -- parse >>= print . solve1
