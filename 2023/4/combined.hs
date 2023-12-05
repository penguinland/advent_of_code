import Control.FromSum(fromEither)
import Data.HashSet(HashSet, fromList, intersection, size)
import Text.Parsec(endBy, eof, many1, parse, sepBy)
import Text.Parsec.Char(char, digit, string)


data Scratch = Scratch{ticketId :: Int, winners :: [Int], nums :: [Int]}


parseGame :: String -> Scratch
parseGame = fromEither (error . show) . parse parseLine ""
  where
    parseLine = do
        string "Card" >> whitespace
        ticketId <- parseNumber
        char ':' >> whitespace
        -- Sneaky trouble here: intuitively it's `sepBy parseNumber whitespace`
		-- followed by `string " | "`. but the sepBy tries matching the
		-- whitespace for the next number, and then the unexpected '|' pushes
		-- it to go on. There ought to be a way to avoid this with
		-- `Parsec.try`, but I haven't figured it out yet.
        winners <- endBy parseNumber whitespace
        char '|' >> whitespace
        nums <- sepBy parseNumber whitespace
        eof
        return Scratch{ticketId=ticketId, winners=winners, nums=nums}
    whitespace = many1 (char ' ')
    parseNumber = many1 digit >>= return . read


winningNums :: Scratch -> HashSet Int
winningNums s = intersection (fromList $ winners s) (fromList $ nums s)


-- Sneaky trouble here: I originally wrote
--     score mempty = 0
--     score x      = 2 ^ (size x - 1)
-- and the compiler warned me that the second one would be unused because the
-- first one always matched. Remember that mempty is not a pattern!
score :: HashSet Int -> Int
score x  = if x == mempty then 0 else 2 ^ (size x - 1)


addCards :: [Int] -> [Int]
addCards = addCards' startingCards
  where
    startingCards = 1 : startingCards
    addCards' _              []             = []
    addCards' (count:counts) (score:scores) = count : addCards' counts' scores
      where
        counts' = applyToN score (+ count) counts
        applyToN 0 _ rest   = rest
        applyToN n f (x:xs) = f x : applyToN (n - 1) f xs


main :: IO()
main = do
    stdin <- getContents
    let games = map parseGame . lines $ stdin
    print . sum . map (score . winningNums) $ games
    print . sum . addCards . map (size . winningNums) $ games
