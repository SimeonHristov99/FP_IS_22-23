main :: IO()
main = do
    print $ isPalindrome 1 == True
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    print $ isPalindrome 121 == True
    print $ isPalindrome 12 == False
    print $ isPalindrome 120 == False
    print $ isPalindrome 12321 == True
    print $ isPalindrome 1221 == True

isPalindrome :: Int -> Bool
isPalindrome n = rev n == n

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)
