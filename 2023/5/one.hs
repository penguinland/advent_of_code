import Control.FromSum(fromEither)
import Data.Maybe(fromMaybe)
import Data.Monoid(Last(..), getLast)
import Text.Parsec(endBy, eof, many, many1, noneOf, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


data Range = Range Integer Integer Integer


translate :: Integer -> Range -> Maybe Integer
translate val (Range dst src len) =
    if src <= val && val < src + len
    then Just $ dst + val - src
    else Nothing


data Mapping = Mapping [Range]


transform :: Mapping -> Integer -> Integer
transform (Mapping ranges) target =
    fromMaybe (error "impossible") . getLast . mconcat . map Last $ ((Just target) : map (translate target) ranges)


parseGame :: String -> ([Integer], [Mapping])
parseGame = fromEither (error . show) . parse parseLine ""
  where
    parseLine = do
        string "seeds: "
        seeds <- sepBy parseNumber whitespace
        eol
        mappings <- many parseMapping
        eof
        return (seeds, mappings)
    parseMapping = do
        eol >> many (noneOf ":") >> char ':' >> eol
        many parseRange >>= return . Mapping
    parseRange = do
        dst <- parseNumber
        whitespace
        src <- parseNumber
        whitespace
        len <- parseNumber
        eol
        return $ Range dst src len
    whitespace = many1 (char ' ')
    eol = char '\n'
    parseNumber = many1 digit >>= return . read


toSoil :: [Mapping] -> Integer -> Integer
toSoil []     v = v
toSoil (m:ms) v = toSoil ms (transform m v)


seedRanges :: [Integer] -> [Integer]
seedRanges [] = []
seedRanges (x:l:xs) = [x..x+l-1] ++ seedRanges xs

main :: IO()
main = do
    stdin <- getContents
    let (seeds, mappings) = parseGame stdin
    print . minimum . map (toSoil mappings) $ seeds
    print . minimum . map (toSoil mappings) . seedRanges $ seeds
