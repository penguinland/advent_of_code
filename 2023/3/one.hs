import Data.Char(digitToInt, isDigit)


isSymbol :: Char -> Bool
isSymbol = and . (map (/=) "1234567890." <*>) . pure


-- In the output, the first bool is whether this digit is adjacent to a symbol,
-- and the second one is whether it is immediately followed by another digit as
-- part of the same number.
findDigits :: [String] -> [(Int, Bool, Bool)]
findDigits schematic = concat $ zipWith3 findDigitsInRow (init $ init schematic)
                                                         (tail $ init schematic)
                                                         (tail $ tail schematic)


findDigitsInRow :: String -> String -> String -> [(Int, Bool, Bool)]
findDigitsInRow above target below = let
    checkColumn a t b = (if isDigit t then Just (digitToInt t) else Nothing,
                         any isSymbol [a, t, b])
    columnData = zipWith3 checkColumn above target below
    checkNeighbors (aVal, aSym) (bVal, bSym) (cVal, cSym) =
        (bVal, or [aSym, bSym, cSym], cVal /= Nothing)
    neighborData = zipWith3 checkNeighbors (init $ init columnData)
                                           (init $ tail columnData)
                                           (tail $ tail columnData)
  in
    -- There must be a better way to do this, but I couldn't think of it...
    map (\(Just x, y, z) -> (x, y, z)) .
        filter (\(x, _, _) -> x /= Nothing) $ neighborData


-- Surely this is just a super-complicated fold, but I couldn't see it.
findPartNumbers :: [(Int, Bool, Bool)] -> [Int]
findPartNumbers = findPartNumbers' 0 False
  where
    findPartNumbers' _ _ [] = []
    findPartNumbers' total bySymbol ((value, bySymbol', continued):rest) = let
        newTotal = 10 * total + value
        nearSymbol = bySymbol || bySymbol'
        maybePrependValue = if nearSymbol then (newTotal :) else id
      in
        if continued
        then findPartNumbers' newTotal nearSymbol rest
        else maybePrependValue $ findPartNumbers rest


pad :: [String] -> [String]
pad schematic = let
    allDots = '.' : allDots
  in
    allDots : map (('.' :) . (++ ".")) schematic ++ [allDots]


main :: IO()
main = getContents >>= print . sum . findPartNumbers . findDigits . pad . lines
