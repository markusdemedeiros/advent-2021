module P02 where

-- Parse input file 
parse :: IO [(String, Int)]
parse = readFile "data/02/data.txt" >>= return . map preparse . lines
    where preparse :: String -> (String, Int)
          preparse = (\(s:i:_) -> (s, read i)) . words

-- Read input data to question 1 movement 
parse_i :: String -> Int -> (Int, Int) -> (Int, Int)
parse_i "down"      n (h, d) = (h, d+n)
parse_i "up"        n (h, d) = (h, d-n)
parse_i "forward"   n (h, d) = (h+n, d)

-- Read input data to question 2 movement 
parse_i' :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
parse_i' "down"      n (h, d, a) = (h, d, a+n)
parse_i' "up"        n (h, d, a) = (h, d, a-n)
parse_i' "forward"   n (h, d, a) = (h+n, d+a*n, a)

solve1 :: [(String, Int)] -> (Int, Int)
solve1 = foldr (uncurry parse_i) (0,0) 

solve2 :: [(String, Int)] -> (Int, Int, Int)
solve2 = foldr (uncurry parse_i') (0,0,0) . reverse

solve :: IO ()
solve = do
    inp <- parse
    parse >>= return . solve1 >>= print . (\(a,b) -> a*b) 
    parse >>= return . solve2 >>= print . (\(a,b,_) -> a*b) 
    return ()
