import Control.FromSum(fromEither)
import Text.Parsec(endBy, eof, many, many1, noneOf, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


parseInput :: String -> ([Int], [Int])
parseInput = fromEither (error . show) . parse parseInput' ""
  where
    parseInput' = do
        string "Time:"
        whitespace
        times <- sepBy parseNumber whitespace
        eol
        string "Distance:"
        whitespace
        distances <- sepBy parseNumber whitespace
        eol
        eof
        return (times, distances)
    whitespace = many1 (char ' ')
    eol = char '\n'
    parseNumber = many1 digit >>= return . read


--waysToBeat :: Int -> Int -> (Int, Int)
waysToBeat :: Int -> Int -> Int
waysToBeat time distance = let
    -- We need to solve x * (time - x) = distance
    -- or -x^2 + time * x - distance = 0
    a = fromIntegral $ -1
    b = fromIntegral $ time
    c = fromIntegral $ -distance
    b2m4ac = sqrt $ b ^ 2 - 4 * a * c
    low  = ceiling $ (-b + b2m4ac) / (2 * a)
    high = floor   $ (-b - b2m4ac) / (2 * a)
    top = min high time
  in
    -- Tying the record isn't good enough: we must beat it. If we would have tied it, subtract 2
    -- from the total.
    if high * low == distance then high - low - 1 else high - low + 1
    --(high, low)


main :: IO()
main = do
    stdin <- getContents
    let (times, distances) = parseInput stdin
    print . product . zipWith waysToBeat times $ distances
