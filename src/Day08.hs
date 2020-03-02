module Day08
    ( part1
    , part2
    )
where

import           Data.List                      ( foldl1'
                                                , minimumBy
                                                )
import           Data.Ord                       ( comparing )
import           Data.List.Split                ( chunksOf )
import           Control.Monad                  ( join
                                                , forM_
                                                )

import           Paths_advent_of_code


part1 :: IO Int
part1 = do
    layers <- parseImages

    let digits = fewestZeros layers
    let ones   = countElem '1' digits
    let twos   = countElem '2' digits

    return $ ones * twos


part2 :: IO String
part2 = do
    colorRows <-
        chunksOf 25 . map toColor . foldl1' getTopColors <$> parseImages

    putStrLn ""
    forM_ colorRows putStrLn
    return "see image ^"


parseImages :: IO [String]
parseImages = do
    (line : _) <- lines <$> (readFile =<< getDataFileName "inputs/day08.txt")

    return $ chunksOf (25 * 6) line


fewestZeros :: [String] -> String
fewestZeros = minimumBy (comparing (countElem '0'))


countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)


getTopColors :: String -> String -> String
getTopColors ""       _        = ""
getTopColors _        ""       = ""
getTopColors (x : xs) (y : ys) = topColor : getTopColors xs ys
    where topColor = if x == '2' then y else x


toColor :: Char -> Char
toColor '0' = '█'
toColor _ = ' '
