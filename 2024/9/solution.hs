ensureFileAtEnd :: [Int] -> [Int]
ensureFileAtEnd contents | length contents `mod` 2 == 0 = contents ++ [0]
                         | otherwise                    = contents


generateLayout :: Int -> Int -> [Int] -> [Maybe Int]
generateLayout _ _ [] = []
generateLayout nextFileId _ [x] = replicate x (Just nextFileId)
generateLayout nextFileId di (fileLength:skipLength:rest) =
    replicate fileLength (Just nextFileId) ++ replicate skipLength Nothing ++
        generateLayout (nextFileId + di) di rest


compact :: [Int] -> [Int]
compact contents = let
    lastFileId = length contents `div` 2
    totalBlocks = sum contents
    forwardLayout = zip [0..] $ generateLayout 0 1 contents
    backwardLayout = zip (map (\x -> totalBlocks - 1 - x) [0..])
                         (generateLayout lastFileId (-1) (reverse contents))
    merge []                _                           = []
    merge _                 []                          = []
    merge ((fi, _):_)       ((bi, _):_)       | fi > bi = []
    merge forwards          ((_, Nothing):bs)           = merge forwards bs
    merge ((_, Just fx):fs) backwards                   = fx : merge fs backwards
    merge ((_, Nothing):fs) ((_, Just bx):bs)           = bx : merge fs bs
  in
    merge forwardLayout backwardLayout


checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]


main :: IO ()
-- Remember to use `init` to strip off the trailing newline at the end
main = getContents >>= print . checksum . compact . ensureFileAtEnd . map (read . (:[])) . init
