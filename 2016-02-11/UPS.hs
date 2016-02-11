module UPS where
import Data.List

parenToInt :: Char -> Int
parenToInt '(' = 1
parenToInt ')' = -1
parenToInt _   = 0

solution1 :: String -> Int
solution1 = sum . fmap parenToInt

solution2 :: String -> Int
solution2 = length . head . filter ((== -1) . sum) . drop 1 . inits . fmap parenToInt
