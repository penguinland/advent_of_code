import Control.FromSum(fromEither)
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
isSafe rules update = let
    pairs = zipWith (,) (init update) (tail update)
    pairOk (a, b) = not . any (== (b, a)) $ rules
  in
    all pairOk pairs


main :: IO ()
main = do
    contents <- getContents
    let (rules, updates) = parseData contents
    print . sum . map findMiddle . filter (isSafe rules) $ updates
