import Control.FromSum(fromEither)
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)
import Text.Parsec((<|>), endBy, eof, many, parse)
import Text.Parsec.Char(char, alphaNum, string)


parseInput :: String -> (String, Map.HashMap String (String, String))
parseInput = fromEither (error . show) . parse parseInput' ""
  where
    parseInput' = do
        directions <- many (char 'L' <|> char 'R')
        eol >> eol
        entries <- endBy parseLine eol
        eof
        return (directions, Map.fromList entries)
    parseLine = do
        node <- parseWord
        string " = ("
        left <- parseWord
        string ", "
        right <- parseWord
        string ")"
        return (node, (left, right))
    parseWord = many alphaNum
    eol = char '\n'


step :: Char -> Map.HashMap String (String, String) -> String -> String
step dir mappings = move dir . fromJust . flip Map.lookup mappings
  where
    move 'L' = fst
    move 'R' = snd
    move _   = error "impossible"


traverseDesert :: (String, Map.HashMap String (String, String)) -> (String -> Bool) -> String -> Int
traverseDesert (dirs, mappings) atEnd = traverse' 0 (cycle dirs)
  where
    traverse' distance _ current | atEnd current = distance
    traverse' distance (dir:dirs) current =
        traverse' (distance + 1) dirs (step dir mappings current)


main :: IO()
main = do
    contents <- getContents >>= return . parseInput
    print . traverseDesert contents (== "ZZZ") $ "AAA"
    print . foldl lcm 1 . map (traverseDesert contents ((== 'Z') . last)) . filter ((== 'A') . last) . Map.keys . snd $ contents
