import GHC.Utils.Misc(nTimes)


blink :: [Int] -> [Int]
blink [] = []
blink (0 : rest) = 1 : blink rest
blink (value : rest) = let
    str = show value
    len = length str
    firstHalf  = read . take (len `div` 2) $ str
    secondHalf = read . drop (len `div` 2) $ str
  in
    if len `mod` 2 == 0
    then firstHalf : secondHalf : blink rest
    else value * 2024 : blink rest


main :: IO ()
main = do
    contents <- getContents
    print . length . nTimes 25 blink . map (read) . words $ contents
    print . length . nTimes 75 blink . map (read) . words $ contents
