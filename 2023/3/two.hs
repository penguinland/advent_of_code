import Data.Char(digitToInt, isDigit)
import Data.List(group)


data Piece = Number Int | Asterisk | Other deriving (Eq, Show)


leadingNumber :: String -> Int
leadingNumber = leadingNumber' 0
  where
    leadingNumber' previous (a:as) | isDigit a =
        leadingNumber' (10 * previous + digitToInt a) as
    leadingNumber' previous _ = previous


-- Each digit will get turned into its own copy of the entire number. For
-- example, ".35*" will become [Other, Number 35, Number 35, Asterisk]
parseRow :: String -> [Piece]
parseRow = parseRow' Nothing
  where
    parseRow' _ ('*':as) = Asterisk : parseRow' Nothing as
    parseRow' Nothing (a:as) | isDigit a =
        let value = leadingNumber (a:as)
        in Number value : parseRow' (Just value) as
    parseRow' (Just value) (a:as) | isDigit a =
        Number value : parseRow' (Just value) as
    parseRow' _ (_:as) = Other : parseRow' Nothing as
    parseRow' _ [] = []


findGears :: [[Piece]] -> [Int]
--findGears :: [[Piece]] -> [((Piece, Piece, Piece),(Piece, Piece, Piece),(Piece, Piece, Piece))]
findGears schematic = concat $ zipWith3 findGearsInRow (init $ init schematic)
                                                       (tail $ init schematic)
                                                       (tail $ tail schematic)


findGearsInRow :: [Piece] -> [Piece] -> [Piece] -> [Int]
--findGearsInRow :: [Piece] -> [Piece] -> [Piece] -> [((Piece, Piece, Piece),(Piece, Piece, Piece),(Piece, Piece, Piece))]
findGearsInRow above target below = let
    cols = zip3 above target below
    spots = zip3 (init $ init cols) (tail $ init cols) (tail $ tail cols)
    isAsterisk (_, (_, x, _), _) = x == Asterisk
    findDistinct = map head . group
    isNumber (Number _) = True
    isNumber _          = False
    getNumber (Number x) = x
    findNumbers ((q, a, z), (w, s, x), (e, d, c)) =
        filter isNumber $ a : d : findDistinct [q, w, e] ++ findDistinct [z, x, c]
  in
    map product . map (map getNumber) . filter ((== 2) . length) . map findNumbers . filter isAsterisk $ spots
    --filter isAsterisk $ spots


pad :: [String] -> [String]
pad schematic = let
    allDots = '.' : allDots
  in
    allDots : map (('.' :) . (++ ".")) schematic ++ [allDots]


main :: IO()
main = getContents >>= print . sum . findGears . map parseRow . pad . lines
--main = getContents >>= print . findGears . map parseRow . pad . lines
