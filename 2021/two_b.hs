import Data.Char(toUpper)

data Movement = Forward Int | Down Int | Up Int deriving Read

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs

move :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
move (aim, x, y) (Forward dx) = (aim,      x + dx, y + aim * dx)
move (aim, x, y) (Down da)    = (aim + da, x,      y)
move (aim, x, y) (Up da)      = (aim - da, x,      y)

main :: IO ()
main = do
    contents <- getContents
    let movements = map (read . capitalize) . lines $ contents
        (aim, x, y) = foldl move (0, 0, 0) movements
    print (x * y)
