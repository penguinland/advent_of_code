import Data.List(sort, transpose)


countOccurrences :: Int -> [Int] -> Int  -- Assumes the list is sorted
countOccurrences target = length . takeWhile (== target) . dropWhile (< target)


main :: IO ()
main = do
    contents <- getContents
    let lists = transpose . map (map read . words) . lines $ contents
        sorted0 = sort (lists !! 0)
        sorted1 = sort (lists !! 1)
    print . sum . map abs $ zipWith (-) sorted0 sorted1
    print . sum . map (\x -> x * countOccurrences x sorted1) $ sorted0
