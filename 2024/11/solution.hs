import qualified Data.MemoCombinators as Memoize


blink :: Int -> Int -> Int
blink = Memoize.memo2 Memoize.integral Memoize.integral blink'
  where
    blink' 0 _     = 1
    blink' n 0     = blink (n - 1) 1
    blink' n value = let
        str = show value
        len = length str
        firstHalf  = read . take (len `div` 2) $ str
        secondHalf = read . drop (len `div` 2) $ str
      in
        if len `mod` 2 == 0
        then blink (n - 1) firstHalf + blink (n - 1) secondHalf
        else blink (n - 1) (value * 2024)


main :: IO ()
main = do
    contents <- getContents
    print . sum . map (blink 25 . read) . words $ contents
    print . sum . map (blink 75 . read) . words $ contents
