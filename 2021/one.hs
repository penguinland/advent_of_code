main :: IO ()
main = do
    contents <- getContents
    let depths = map read . lines $ contents
        diffs = zipWith (-) (tail depths) (init depths)
        total = length . filter (> 0) $ diffs
    print total
