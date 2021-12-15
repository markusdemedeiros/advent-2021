module P03 where

import Data.List (transpose, partition)

-- Helper: Little endian boolean sting to int
be_bool_to_int :: [Bool] -> Int
be_bool_to_int = foldl (\acc b -> fromEnum b + 2*acc) 0

solve1 :: [String] -> Int
solve1 i = (be_bool_to_int gamma_bits) * (be_bool_to_int (map not gamma_bits))
    where gamma_bits =  map ((> length i) . (*2) . length . filter (== '1')) . transpose $ i


solve2 :: [String] -> Int
solve2 i = undefined
    where 
          b_to_s True  = "1"
          b_to_s False = "0"

--                         most popular string
--                                 most unpopular string
pop_search :: [String] -> ([Bool], [Bool])
pop_search [] = ([],[])
pop_search s = if (length s_1s > length s_0s)
                then (True:(fst ps_1),  False:(snd ps_0))
                else (False:(fst ps_0), True:(snd ps_1))
    where s_1s = filter ((=='1') . (!!0)) $ s
          s_0s = filter ((=='0') . (!!0)) $ s
          ps_1 = pop_search (map tail s_1s)
          ps_0 = pop_search (map tail s_0s)
          

-- Break popularity ties in the favour of True
most_popular :: [[Bool]] -> [Bool]
most_popular []     = error "no most popular element"
most_popular [[]]   = []
most_popular s      = undefined

ps :: [[Bool]] -> ([[Bool]], [[Bool]])
ps l = partition head l


-- Combine non-equal length cases togethah
-- equal length case defers to bool
-- Arranges by length, leaves unchanged if tie
--
--         searching-for-popular? (break ties to True)
--                  --Starts with True
--                       -- Starts with False
arr_pop :: Bool -> ([a], [a]) -> [a]
arr_pop b l
    | length (fst l) < length (snd l)   = undefined -- (snd l, fst l)
    | otherwise                         = undefined -- l






-- mpv, breaking in favour of True
most_popular_value :: [Bool] -> Bool
most_popular_value l = count True >= count False
    where count :: Bool -> Int
          count x = length . filter (== x) $ l




test_strs :: [[Bool]]
test_strs = map (map c_to_b) $ 
            [ "00100", 
              "11110",
              "10110",
              "10111",
              "10101",
              "01111",
              "00111",
              "11100",
              "10000",
              "11001",
              "00010",
              "01010"]
    where c_to_b '1' = True
          c_to_b '0' = False


-- Most popular: 10111

          
-- map ((> length i) . (*2) . length . filter (== '1')) . transpose $ i


parse :: IO [String]
parse = readFile "data/03/data.txt" >>= return . lines

solve :: IO ()
solve = do
    inp <- parse
    return ()
    parse >>= print . solve1
