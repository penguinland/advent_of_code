import Data.Char(digitToInt)
import Data.List(transpose)

bitsToNum :: [Int] -> Int
bitsToNum = foldl (\total bit -> total * 2 + bit) 0

main :: IO ()
main = do
    contents <- getContents
    let bits = map (map digitToInt) . lines $ contents
        totals = map sum . transpose $ bits
        threshold = length bits `div` 2
        gammaBits = map (\x -> if x > threshold then 1 else 0) totals
        epsilonBits = map (1 -) gammaBits
    print $ (bitsToNum gammaBits) * (bitsToNum epsilonBits)
