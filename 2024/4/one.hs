import Data.List(transpose)
import Data.List.Extra((!?))
import Data.List.Utils(split)
import Data.Maybe(fromJust)


rotate :: [[a]] -> [[a]]
rotate = map reverse . transpose  -- Rotates the block 90 degrees clockwise


-- Example input:   a b c
--                  d e f
--                  g h i
--
-- Example output:    a
--                   d b
--                  g e c
--                   h f
--                    i
diagonals :: [String] -> [String]
diagonals block = let
    rowCount = length block
    colCount = length $ head block
    getFrom row col = (block !? row) >>= (!? col)
    getDiag offset = takeWhile (not . null) . dropWhile null $ getDiag' offset 0
      where
        getDiag' row col = getFrom row col : getDiag' (row - 1) (col + 1)
  in
    map (fromJust . sequence . getDiag) [0..(rowCount - 1 + colCount - 1)]


countForwardInLine :: String -> Int
countForwardInLine = subtract 1 . length . split "XMAS"


countInLine :: String -> Int
countInLine text = countForwardInLine text + countForwardInLine (reverse text)


countInLines :: [String] -> Int
countInLines = sum . map countInLine


searchWords :: [String] -> Int
searchWords block = sum . map countInLines $ [ block                       -- forwards and backwards
                                             , transpose block             -- up and down
                                             , diagonals block             -- rising diagonals
                                             , diagonals . rotate $ block  -- falling diagonals
                                             ]


main :: IO ()
main = getContents >>= print . searchWords . lines
