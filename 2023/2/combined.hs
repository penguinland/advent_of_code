import Text.Parsec(choice, endBy, eof, many, parse, sepBy, try)
import Text.Parsec.Char(char, digit, string)
import Text.ParserCombinators.Parsec(GenParser)


data Showing = Showing {redCount :: Int, greenCount :: Int, blueCount :: Int}

instance Semigroup Showing where
    x <> y = Showing{redCount  =max (redCount   x) (redCount   y),
                     blueCount =max (blueCount  x) (blueCount  y),
                     greenCount=max (greenCount x) (greenCount y)}

instance Monoid Showing where
    mempty = Showing 0 0 0


data Game = Game {gameNumber :: Int, showings :: [Showing]}


parseInput :: GenParser Char st [Game]
parseInput = do
    result <- endBy parseLine (char '\n')
    eof
    return result

parseLine :: GenParser Char st Game
parseLine = do
    string "Game "
    gameNumber <- parseNumber
    string ": "
    showings <- sepBy parseShowing (string "; ")
    return $ Game gameNumber showings

-- Surely this is built in somewhere, but I couldn't find it.
parseNumber :: GenParser Char st Int
parseNumber = do
    digits <- many digit
    return $ read digits

parseShowing :: GenParser Char st Showing
parseShowing = do
    colorCounts <- sepBy (choice . map try $ parseColors) (string ", ")
    return $ mconcat colorCounts
  where
    makeColorParser (name, updater) = do
        count <- parseNumber
        char ' '
        string name
        return $ updater count
    parseColors = map makeColorParser [("red",   \count -> mempty{redCount  =count}),
                                       ("green", \count -> mempty{greenCount=count}),
                                       ("blue",  \count -> mempty{blueCount =count})]


makeGames :: String -> [Game]
makeGames input = case parse parseInput "malformed input" input of
    -- This ought to just be fromRight from MissingH, but I didn't bother setting up Cabal or Stack
    -- so I can't install non-default packages.
    Left e  -> error (show e)
    Right g -> g


isPossible :: Game -> Bool
isPossible = let
    checks = map (\(f, c) -> (<= c) . f) [(redCount, 12), (greenCount, 13), (blueCount, 14)]
    checksPass = and . (checks <*>) . pure
  in
    all checksPass . showings


power :: Game -> Int
power = product . ([redCount, greenCount, blueCount] <*>) . pure . mconcat . showings


main :: IO()
main = do
    stdin <- getContents
    let games = makeGames stdin
    print . sum . map gameNumber . filter isPossible $ games  -- Part 1
    print . sum . map power $ games  -- Part 2
