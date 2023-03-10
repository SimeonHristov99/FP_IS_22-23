main :: IO()
main = do
    print $ digitalRoot 16 == 7
    -- => 1 + 6
    -- => 7
    print $ digitalRoot 942 == 6
    -- => 9 + 4 + 2
    -- => 15 ...
    -- => 1 + 5
    -- => 6
    print $ digitalRoot 132189 == 6
    print $ digitalRoot 493193 == 2

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)

digitalRoot :: Int -> Int
digitalRoot n
 | n < 10 = n
 | otherwise = digitalRoot $ sumDigits n
