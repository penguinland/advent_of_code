isSafe :: [Int] -> Bool
isSafe ns = let
    diffs = zipWith (-) (tail ns) (init ns)
    isIncreasing = all (> 0) diffs
    isDecreasing = all (< 0) diffs
    smallChanges = all (<= 3) . map abs $ diffs
  in
    (isIncreasing || isDecreasing) && smallChanges


removeAnyOne :: [Int] -> [[Int]]
removeAnyOne [] = []
removeAnyOne (x:xs) = xs : (map (x:) $ removeAnyOne xs)


main :: IO ()
main = do
    contents <- getContents
    let reports = map (map read . words) . lines $ contents
    print . length . filter isSafe $ reports
    print . length . filter (any isSafe . removeAnyOne) $ reports
