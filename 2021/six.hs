import Data.List(group, sort)
import Data.Vector as Vector

-- This is built into GHC.Utils.Misc, but I'm having trouble installing anything
-- today.
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _  a = a
nTimes n fn a = nTimes (n - 1) fn (fn a)

reset :: Int -> [Int] -> [Int]
reset extras (a : b : c : d : e : f : rest) =
    a : b : c : d : e : (f + extras) : rest
--reset extras other =
    --let
    --take 5 other

age :: [Int] -> [Int]
age fish =
  reset (head fish) $ tail fish ++ [head fish]

main :: IO ()
main = do
    input <- getLine
    let input_ints = (read :: String -> [Int]) $ "[" ++ input ++ "]"
        fish = map length . group . sort $ input_ints
    print . sum . nTimes 80 age $ fish
