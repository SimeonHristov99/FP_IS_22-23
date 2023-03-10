main :: IO()
main = do
    print $ findMax 55345 == 5
    print $ findMax 14752 == 7
    print $ findMax 329450 == 9
    print $ findMax 9521 == 9
    print $ findMax 5 == 5

findMax :: Int -> Int
findMax n = helper (div n 10) (mod n 10)
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result
     | mod leftover 10 > result = helper (div leftover 10) (mod leftover 10)
     | otherwise = helper (div leftover 10) result