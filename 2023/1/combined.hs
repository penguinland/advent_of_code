import Data.Char(digitToInt, isDigit)
import Data.List(isPrefixOf, tails)
import Data.Maybe(catMaybes)
import Data.Monoid(First(..), getFirst)


toOutputOne :: String -> Int
toOutputOne line = let
    digits = map digitToInt . filter isDigit $ line
  in
    10 * head digits + last digits


startingDigit :: String -> Maybe Int
startingDigit []                = Nothing
startingDigit (n:_) | isDigit n = Just $ digitToInt n
startingDigit line              = let
    startingNamedDigit (name, value) =
        if isPrefixOf name line then Just value else Nothing
    digitNames = [("zero", 0), ("one", 1), ("two", 2), ("three", 3),
                  ("four", 4), ("five", 5), ("six", 6), ("seven", 7),
                  ("eight", 8), ("nine", 9)]
  in
    getFirst . mconcat . map (First . startingNamedDigit) $ digitNames


toOutputTwo :: String -> Int
toOutputTwo line = let
    digits = catMaybes . map startingDigit . tails $ line
  in
    10 * head digits + last digits


main :: IO()
main = do
    contents <- getContents
    print . sum . map toOutputOne . lines $ contents
    print . sum . map toOutputTwo . lines $ contents
