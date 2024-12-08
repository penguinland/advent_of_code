import Control.FromSum(fromEither)
import Text.Parsec(eof, many, many1, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


parseData :: String -> [(Int, [Int])]
parseData = fromEither (error . show) . parse parseFile ""
  where
    parseFile = do
        rules <- many parseRule
        eof
        return rules
    parseRule = do
        target <- number
        string ": "
        components <- number `sepBy` (char ' ')
        char '\n'
        return (target, components)
    number = many1 digit >>= (return . read)


canBeMade :: (Int, [Int]) -> Bool
canBeMade (target, components) = any (== target) $ allPossible components
  where
    allPossible [] = []
    allPossible [x] = [x]
    allPossible (x : y : xs) = allPossible ((x + y)        : xs) ++
                               allPossible ((x * y)        : xs) ++
                               allPossible ((x `concat` y) : xs)
    concat x y = read (show x ++ show y)


main :: IO ()
main = do
    contents <- getContents
    print . sum . map fst . filter canBeMade . parseData $ contents
