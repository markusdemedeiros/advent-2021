module P01 where


solve1 :: [Int] -> Int
solve1 i = length . filter (<0) $ zipWith (-) i (tail i)

solve2 :: [Int] -> Int
solve2 i = solve1 . zipWith (+) i $ zipWith (+) (tail i) (tail (tail i))

parse :: IO [Int]
parse = readFile "data/01/data.txt" >>= return . map read . lines

solve :: IO ()
solve = do
    inp <- parse
    parse >>= print . solve1
    parse >>= print . solve2 
