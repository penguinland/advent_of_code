import Data.Char(digitToInt, isDigit)
import Data.List(group)
import Data.Maybe(fromMaybe)


data Piece = Number Int | Asterisk | Other deriving (Eq, Show)


leadingNumber :: String -> Int
leadingNumber = read . takeWhile isDigit


-- Each digit will get turned into its own copy of the entire number. For
-- example, ".35*" will become [Other, Number 35, Number 35, Asterisk]
parseRow :: String -> [Piece]
parseRow = parseRow' Nothing
  where
    parseRow' _ ('*':as) = Asterisk : parseRow as
    parseRow' previous (a:as) | isDigit a =
        let value = fromMaybe (leadingNumber (a:as)) previous
        in Number value : parseRow' (Just value) as
    parseRow' _ (_:as) = Other : parseRow as
    parseRow' _ [] = []


findGears :: [[Piece]] -> [Int]
findGears schematic = concat $ zipWith3 findGearsInRow (init $ init schematic)
                                                       (tail $ init schematic)
                                                       (tail $ tail schematic)


findGearsInRow :: [Piece] -> [Piece] -> [Piece] -> [Int]
findGearsInRow above target below = let
    cols = zip3 above target below
    spots = zip3 (init $ init cols) (tail $ init cols) (tail $ tail cols)
    isAsterisk (_, (_, x, _), _) = x == Asterisk
    findDistinct = map head . group
    isNumber (Number _) = True
    isNumber _          = False
    getNumber (Number x) = x
    findNumbers ((q, a, z), (w, s, x), (e, d, c)) = filter isNumber $
        a : d : findDistinct [q, w, e] ++ findDistinct [z, x, c]
  in
    map (product . map getNumber) . filter ((== 2) . length) .
        map findNumbers . filter isAsterisk $ spots


pad :: [String] -> [String]
pad schematic = let
    allDots = '.' : allDots
  in
    allDots : map (('.' :) . (++ ".")) schematic ++ [allDots]


main :: IO()
main = getContents >>= print . sum . findGears . map parseRow . pad . lines
