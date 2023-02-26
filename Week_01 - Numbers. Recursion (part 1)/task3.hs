main :: IO()
main = do
    print $ factRec 11 == 39916800
    -- print $ factRec (-11) -- error: x was negative
    print $ factIter 11 == 39916800
    -- print $ factIter (-11) -- error: x was negative

factRec :: Int -> Int
factRec 0 = 1
factRec n
 | n < 0 = error "x was negative"
 | otherwise = n * factRec (n - 1)

-- factIter 0
-- > helper 0 1
-- > helper 0 result=1
-- > result=1

factIter :: Int -> Int
factIter n
 | n < 0 = error "x was negative"
 | otherwise = helper n 1
 where
    helper 0 result = result
    helper leftover result = helper (leftover - 1) (leftover * result)

-- factIter 5
-- > helper 5 1
-- > helper 4 result=(5 * 1)
-- > helper 3 result=(5 * 4 * 1)
-- > helper 3 result=(5 * 4 * 3)
-- ...
-- > helper 0 result=(5 * 4 * 3 * 2 * 1 * 1)
