main :: IO()
main = do
    print $ areNotEqualOneLine 5 2 == True
    print $ areNotEqualOneLine 5 5 == False

    print $ areNotEqualGuards 5 2 == True
    print $ areNotEqualGuards 5 5 == False

    print $ inside 1 5 4 == True -- start = 1, finish = 5, x = 4
    print $ inside 5 1 4 == True
    print $ inside 10 50 20 == True
    print $ inside 10 50 1 == False

inside :: Int -> Int -> Int -> Bool
inside start finish x = (min start finish) <= x && x <= (max start finish)

areNotEqualOneLine :: Int -> Int -> Bool
-- areNotEqualOneLine x y = not $ x == y
areNotEqualOneLine x y = x /= y

areNotEqualGuards :: Int -> Int -> Bool
areNotEqualGuards x y
 | x /= y = True
 | otherwise = False
