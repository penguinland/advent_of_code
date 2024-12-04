import Data.List.Extra((!?))
import Data.Maybe(fromMaybe)


searchWords :: [String] -> Int
searchWords block = let
    getFrom x y = (block !? y) >>= (!? x)
    equals x y v = fromMaybe False $ getFrom x y >>= (return . (== v))
    forwardDiag x y dx dy = equals (x + dx) (y + dy) 'M' &&
                            equals (x - dx) (y - dy) 'S'
    diag x y dx dy = forwardDiag x y dx dy || forwardDiag x y (-dx) (-dy)
    cross x y = diag x y 1 1 && diag x y 1 (-1)
    findAt x y = equals x y 'A' && cross x y
  in
    length . filter id $ [findAt x y | x <- [0..(length block - 1)],
                                       y <- [0..(length (head block) - 1)]]


main :: IO ()
main = getContents >>= print . searchWords . lines
