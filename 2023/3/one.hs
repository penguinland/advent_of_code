import Data.Char(digitToInt, isDigit)


isSymbol :: Char -> Bool
isSymbol = and . (map (==) "1234567890." <*>) . pure


-- In the output, the first bool is whether this digit is adjacent to a symbol,
-- and the second one is whether it is immediately followed by another digit as
-- part of the same number.
findDigits :: [String] -> [(Int, Bool, Bool)]
findDigits schematic = concat $ zipWith3 findDigitsInRow (tail $ tail schematic)
                                                         (tail $ init schematic)
                                                         (init $ init schematic)


findDigitsInRow :: String -> String -> String -> [(Int, Bool, Bool)]
findDigitsInRow above target below = let
    checkColumn a t b = (if isDigit t then Just (digitToInt t) else Nothing,
                         any isSymbol [a, t, b])
    columnData = zipWith3 checkColumn above target below
    checkNeighbors (aVal, aSym) (bVal, bSym) (cVal, cSym) =
        (bVal, or [aSym, bSym, cSym], cVal /= Nothing)
    neighborData = zipWith3 checkNeighbors (tail $ tail columnData)
                                           (init $ tail columnData)
                                           (init $ init columnData)
  in
    -- There must be a better way to do this, but I couldn't think of it...
    map (\(Just x, y, z) -> (x, y, z)) .
        filter (\(x, _, _) -> x /= Nothing) $ neighborData


-- Surely this is just a super-complicated fold, but I couldn't see it.
findPartNumbers :: [(Int, Bool, Bool)] -> [Int]
findPartNumbers = findPartNumbers' 0 False
  where
    appendDigit total value = 10 * total + value
    findPartNumbers' _ _ [] = []
    findPartNumbers' total bySymbol ((value, bySymbol', False):rest) =
        (if bySymbol || bySymbol' then (appendDigit total value :) else id) $
            findPartNumbers' 0 False rest
    findPartNumbers' total bySymbol ((value, bySymbol', True ):rest) =
        findPartNumbers' (appendDigit total value) (bySymbol || bySymbol') rest

partNumbers :: [String] -> [Int]
partNumbers schematic = let
    allDots = '.' : allDots
    paddedSchematic = allDots : map (('.' :) . (++ ".")) schematic ++ [allDots]
    digits = findDigits paddedSchematic
  in
    findPartNumbers digits


main :: IO()
main = do
    getContents >>= print . sum . partNumbers . lines
