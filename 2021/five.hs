import Data.List(sort)

data Line = Line (Int, Int) (Int, Int) deriving Show

parseLine :: String -> Line
parseLine input =
    let pieces = words input
    in Line (read $ "(" ++ pieces !! 0 ++ ")")
            (read $ "(" ++ pieces !! 2 ++ ")")

{-
-- First attempt: works but is hard to read
lineToPoints :: Line -> [(Int, Int)]
lineToPoints (Line (x1, y1) (x2, y2)) | x1 == x2 =
    map (\y -> (x1, y)) [min y1 y2..max y1 y2]
lineToPoints (Line (x1, y1) (x2, y2)) | y1 == y2 =
    map (\x -> (x, y1)) [min x1 x2..max x1 x2]
--lineToPoints _ = []  -- Skip non-vertical, non-horizontal lines for now
lineToPoints (Line (x1, y1) (x2, y2)) | (x1 < x2) == (y1 < y2) =
    zipWith (,) [min x1 x2..max x1 x2] [min y1 y2..max y1 y2]
lineToPoints (Line (x1, y1) (x2, y2)) =  -- We've got the other diagonal
    zipWith (,) [min x1 x2..max x1 x2] [max y1 y2, (max y1 y2 - 1)..min y1 y2]
-}

-- This feels like it ought to be built in somewhere, but I can't find it
orderingToNum :: Ordering -> Int
orderingToNum EQ = 0
orderingToNum LT = 1
orderingToNum GT = -1

lineToPoints :: Line -> [(Int, Int)]
lineToPoints (Line (x1, y1) (x2, y2)) = let
    dx = orderingToNum $ compare x1 x2
    dy = orderingToNum $ compare y1 y2
    xs = iterate (+ dx) x1
    ys = iterate (+ dy) y1
    xSpread = abs (x1 - x2)
    ySpread = abs (y1 - y2)
    total = 1 + max xSpread ySpread
  in
    --if (dx /= 0) && (dy /= 0) then [] else  -- For part a
    take total $ zipWith (,) xs ys

countDuplicates :: [(Int, Int)] -> Int
countDuplicates points = let
    accumulate (total, lastPoint, alreadyDuplicated) nextPoint =
        let update = if lastPoint == nextPoint && not alreadyDuplicated
                     then 1 else 0
        in (total + update, nextPoint, lastPoint == nextPoint)
    (result, _, _) = foldl accumulate (0, (-1, -1), False) points
  in
    result

main :: IO ()
main = do
    contents <- getContents
    print . countDuplicates . sort . concatMap lineToPoints . map parseLine .
        lines $ contents
