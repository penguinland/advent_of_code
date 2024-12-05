import Data.Char(digitToInt)

bitsToNum :: [Int] -> Int
bitsToNum = foldl (\total bit -> total * 2 + bit) 0

-- This feels like it ought to be built-in somewhere, but I can't find it.
boolToNum :: Bool -> Int
boolToNum True  = 1
boolToNum False = 0

mostCommonBit :: [[Int]] -> Int -> Int
mostCommonBit bits index =
    let relevantBits = map (!! index) bits
        -- The threshold should be rounded up for odd lengths. We don't use
        -- `round` because it rounds halves to the nearest even number (i.e.,
        -- half the time it'll round up and half the time it'll round down).
        threshold = (length relevantBits + 1) `div` 2
    in boolToNum $ sum relevantBits >= threshold

leastCommonBit :: [[Int]] -> Int -> Int
leastCommonBit bits index = 1 - mostCommonBit bits index

filterForBits :: ([[Int]] -> Int -> Int) -> [[Int]] -> [Int]
filterForBits = filterForBits' 0
  where
    filterForBits' _ _ [onlyOneLeft] = onlyOneLeft
    filterForBits' index bitSelector bits = let
        bitToFind = bitSelector bits index
        remainingBits = filter ((== bitToFind) . (!! index)) bits
      in
        -- If we encounter an ambiguous case where we run out of bits before we
        -- have only 1 item left, we'll crash with errors indexing past the end
        -- of the list. It's not worth the hassle to prevent that; presumably
        -- all inputs will be well-defined.
        filterForBits' (index + 1) bitSelector remainingBits

main :: IO ()
main = do
    contents <- getContents
    let bits = map (map digitToInt) . lines $ contents
        oxygenRating = filterForBits mostCommonBit  bits
        co2Rating    = filterForBits leastCommonBit bits
    print $ (bitsToNum oxygenRating) * (bitsToNum co2Rating)
