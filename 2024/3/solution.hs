module Main (main) where

import Control.FromSum(fromEither)
import Text.Parsec(eof, many, many1, parse, try, choice)
import Text.Parsec.Char(anyChar, char, digit, string)


data Instruction = Do | Dont | Prod Int | Garbage


parseData :: String -> [Instruction]
parseData = fromEither (error . show) . parse parseLine ""
  where
    parseLine = do
        products <- many (choice [ try parseMul
                                 , try parseDo
                                 , try parseDont
                                 , other])
        _ <- eof
        return products
    parseDo = string "do()" >> return Do
    parseDont = string "don't()" >> return Dont
    parseMul = do
        _ <- string "mul("
        x <- number
        _ <- char ','
        y <- number
        _ <- char ')'
        return . Prod $ x * y
    number = many1 digit >>= return . read
    other = anyChar >> return Garbage


switchOn :: [Instruction] -> [Int]
switchOn = switchOn' True
  where
    switchOn' _  [] = []
    switchOn' _  (Do  :xs)       = switchOn' True  xs
    switchOn' _  (Dont:xs)       = switchOn' False xs
    switchOn' on (Garbage:xs)    = switchOn' on xs
    switchOn' on ((Prod val):xs) = (if on then (val:) else id) $ switchOn' on xs


main :: IO ()
main = getContents >>= print . sum . switchOn . parseData
