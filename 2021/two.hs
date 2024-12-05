import Data.Char(toUpper)

data Movement = Forward Int | Down Int | Up Int deriving Read

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs

move :: (Int, Int) -> Movement -> (Int, Int)
move (x, y) (Forward dx) = (x + dx, y)
move (x, y) (Down dy)    = (x,      y + dy)
move (x, y) (Up dy)      = (x,      y - dy)

main :: IO ()
main = do
    contents <- getContents
    let movements = map (read . capitalize) . lines $ contents
        (x, y) = foldl move (0, 0) movements
    print (x * y)
