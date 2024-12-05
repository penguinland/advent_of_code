main :: IO ()
main = do
    contents <- getContents
    let depths = map read . lines $ contents
        windows = zipWith3 (\x y z -> x + y + z)
                           (tail . tail $ depths)
                           (tail . init $ depths)
                           (init . init $ depths)
        diffs = zipWith (-) (tail windows) (init windows)
        total = length . filter (> 0) $ diffs
    print total
