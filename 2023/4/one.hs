import Control.FromSum(fromEither)
import Data.Either(fromRight)
import Data.HashSet(HashSet, fromList, intersection, size)
import Text.Parsec(endBy, eof, many1, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


data Scratch = Scratch{ticketId :: Int, winners :: [Int], nums :: [Int]}


parseGame :: String -> Scratch
parseGame = fromEither (error . show) . parse parseLine ""
  where
    parseLine = do
        string "Card"
        whitespace
        ticketId <- parseNumber
        char ':'
        whitespace
        winners <- endBy parseNumber whitespace
        string "|"
        whitespace
        nums <- sepBy parseNumber whitespace
        eof
        return Scratch{ticketId=ticketId, winners=winners, nums=nums}
    whitespace = many1 (char ' ')
    parseNumber = many1 digit >>= return . read


winningNums :: Scratch -> HashSet Int
winningNums s = intersection (fromList $ winners s) (fromList $ nums s)


score :: HashSet Int -> Int
score x  = if x == mempty then 0 else 2 ^ (size x - 1)


main :: IO()
main = do
    stdin <- getContents
    let games = map parseGame . lines $ stdin
    print . sum . map (score . winningNums) $ games
