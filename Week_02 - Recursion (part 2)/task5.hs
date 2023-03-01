main :: IO()
main = do
    print $ sumDivs 0 == 0
    print $ sumDivs 1 == 1
    print $ sumDivs 6 == 12 -- 1 + 2 + 3 + 6
    print $ sumDivs 12345 == 19776

sumDivs :: Int -> Int
sumDivs 0 = 0
sumDivs n = helper 1 
 where 
    helper d 
     | d > n = 0
     | mod n d == 0 = d + helper (d + 1)
     | otherwise = helper (d + 1)