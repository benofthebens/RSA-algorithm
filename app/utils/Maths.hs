module Utils.Maths (isqrt, isPrime, isCoprime, getRandomCoprimeOf, getRandomPrime, egcd, invMod) where
import System.Random

-- Define an integer sqrt function
isqrt :: Integer -> Integer
isqrt num = floor(sqrt(fromIntegral num))

-- Define a function to check if a number is prime
isPrime :: Integer -> Bool
isPrime num = 
    if num > 1 then 
        null [x | x <- [2..isqrt num], num `mod` x == 0] else False

-- Define a function to check if 2 numbers are coprime
-- Where: p and q are Integers 
isCoprime :: Integer -> Integer -> Bool
isCoprime p q = gcd p q == 1

-- Define a function to get random prime numbers in a range
getRandomPrime :: Integer -> Integer -> IO Integer
getRandomPrime from to = do
    num <- randomRIO (from, to)
    if isPrime num then return num else getRandomPrime from to

-- Define a function to get a Random coprime of 2 integers p and q
getRandomCoprimeOf :: Integer -> Integer -> Integer -> Integer ->IO Integer
getRandomCoprimeOf p q from to = do 
    num <- randomRIO (from, to)
    if isCoprime p num && isCoprime num q
        then return num 
        else getRandomCoprimeOf p q from to

-- The greatest common divisor (gcd) of two integers a and b, but it also finds integers x and y 
-- where:
-- a * x + b * y = gcd(a, b)
-- g = gcd(a,b)
egcd :: Integer->Integer->(Integer, Integer, Integer)
-- Define base case
egcd a 0 = (a, 1, 0)
egcd a b = 
    let (g, x1, y1) = egcd b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1
    in (g, x, y)

-- Define an inverse Mod function using the egcd
invMod :: Integer -> Integer -> Maybe Integer
invMod a b 
    | g /= 1    = Nothing
    | otherwise = Just (x `mod` b) 
    where (g, x, _) = egcd a b

phi :: Integer -> Integer -> Integer 
phi p q = (p - 1) * (q - 1)

