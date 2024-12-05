import Control.FromSum(fromEither)
import Data.List(sortBy)
import GHC.Utils.Misc(isSortedBy)
import Text.Parsec(eof, many, many1, parse, sepBy)
import Text.Parsec.Char(char, digit)


parseData :: String -> ([(Int, Int)], [[Int]])
parseData = fromEither (error . show) . parse parseFile ""
  where
    parseFile = do
        rules <- many parseRule
        char '\n'
        updates <- many parseUpdate
        eof
        return (rules, updates)
    parseRule = do
        earlier <- number
        char '|'
        later <- number
        char '\n'
        return (earlier, later)
    parseUpdate = do
        pages <- number `sepBy` (char ',')
        char '\n'
        return pages
    number = many1 digit >>= (return . read)


findMiddle :: [Int] -> Int
findMiddle []  = error "even length update!?"
findMiddle [x] = x
findMiddle xs  = findMiddle . tail . init $ xs


isSafe :: [(Int, Int)] -> [Int] -> Bool
isSafe rules = isSortedBy (order rules)


order :: [(Int, Int)] -> Int -> Int -> Ordering
order rules a b | elem (a, b) rules = LT
order rules a b | elem (b, a) rules = GT
order _     a b | a == b            = EQ
order _     _ _ | otherwise         = error "partial ordering"


main :: IO ()
main = do
    contents <- getContents
    let (rules, updates) = parseData contents
    print . sum . map findMiddle . filter (isSafe rules) $ updates
    print . sum . map (findMiddle . sortBy (order rules)) .
        filter (not . isSafe rules) $ updates
