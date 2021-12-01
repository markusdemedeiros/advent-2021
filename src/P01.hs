module P01 where

main :: IO ()
main = do 
    inp <- readFile "data/01/data.txt"
    putStrLn . show . solve1 . map read . lines $ inp
    putStrLn . show . solve2 . map read . lines $ inp


solve1 :: [Int] -> Int
solve1 i = length . filter (<0) $ zipWith (-) i (tail i)

solve2 :: [Int] -> Int
solve2 i = solve1 . zipWith (+) i $ zipWith (+) (tail i) (tail (tail i))
