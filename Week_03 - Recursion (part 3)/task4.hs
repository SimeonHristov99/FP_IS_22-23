main :: IO()
main = do
    print $ sumNumbers 1 9 == 45
    print $ sumNumbers 199 203 == 200
    print $ sumNumbers 219 225 == 663
    print $ sumNumbers 225 219 == 663

isDescending :: Int -> Bool
isDescending n = n < 10 || mod n 10 <= mod (div n 10) 10 && isDescending (div n 10)
--  | n<10 = True
--  | mod n 10 <= mod (div n 10) 10 = isDescending (div n 10)
--  | otherwise = False 

sumNumbers :: Int -> Int -> Int
sumNumbers x y = helper (min x y) (max x y)
 where
    helper :: Int -> Int -> Int
    helper realStart realEnd
     | realStart > realEnd = 0
     | isDescending realStart = realStart + helper (realStart + 1) realEnd
     | otherwise = helper (realStart + 1) realEnd