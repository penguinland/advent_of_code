import Data.HashSet(fromList, intersection, size)
import Text.Parsec(eof, many1, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


data Scratch = Scratch{ticketId :: Int, winners :: [Int], nums :: [Int]}


parseGame :: String -> Scratch
parseGame input = case parse parseLine "malformed" input of
    Left e -> error (show e)
    Right g -> g
  where
    parseLine = do
        string "Card"
        whitespace
        ticketId <- parseNumber
        char ':'
        whitespace
        winners <- sepBy parseNumber whitespace
        string " |"
        whitespace
        nums <- sepBy parseNumber whitespace
        eof
        return Scratch{ticketId=ticketId, winners=winners, nums=nums}
    whitespace = many1 (char ' ')
    parseNumber = many digit >>= return . read


winningNums :: Scratch -> [Int]
winningNums s = intersection (fromList $ winners s) (fromList $ nums s)


score :: [Int] -> Int
score 0 = 0
score x = 2 ^ (x - 1)


main :: IO()
main = getContents >>= print . sum . map (score . winningNums . parseGame) . lines
