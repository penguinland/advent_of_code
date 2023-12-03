partNumbers :: [String] -> [Int]
partNumbers schematic = let
    allDots = '.' : allDots
    paddedSchematic = allDots : map (('.' :) . (++ ".")) schematic ++ [allDots]
  in
    [1,2,3]


main :: IO()
main = do
    getContents >>= print . sum . partNumbers . lines
