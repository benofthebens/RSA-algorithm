module Main where
import System.Random
import Data.Char

isqrt :: Integer -> Integer
isqrt n = floor (sqrt (fromIntegral n))

isPrime :: Integer -> Bool
isPrime n = if n > 1 then null [x | x <- [2..isqrt n], n `mod` x == 0] else False
    
getPrime = do 
    num <- randomRIO (1 :: Integer, 1024 :: Integer)
    if isPrime num then return num else getPrime

isCoprime p q = gcd p q == 1
getCoprime p q = do 
    num <- randomRIO (1 :: Integer, 1024 :: Integer)
    if isCoprime p num && isCoprime num q && num < p*q then return num else getCoprime p q

egcd :: Integer->Integer->(Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b = 
    let (g, x1, y1) = egcd b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1
    in (g, x, y)

invMod a m 
    | g /= 1    = Nothing
    | otherwise = Just (x `mod` m) 
    where (g, x, _) = egcd a m

strToInt :: String -> Integer -> [Integer]
strToInt str n = map(\c -> toInteger (ord c) `mod` n) str

encrypt :: Integer -> Integer -> Integer -> Integer
encrypt m e n =
    (m ^ e) `mod` n

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt c d n = 
    (c ^ d) `mod` n
    
intToStr :: [Integer] -> [Char]
intToStr intarr = map(\c -> (chr (fromIntegral c))) intarr
    
main :: IO ()
main = do 
    prime1 <- getPrime
    prime2 <- getPrime
    e <- getCoprime (prime1-1) (prime2-1)
    let n = prime1 * prime2
    let d = case invMod e ((prime1-1)*(prime2-1)) of
              Just d' -> d'
              Nothing -> error "No modular inverse found"    
    
    let encryptedMessage = map (\m -> encrypt m e n) (strToInt "Hello" n)
    let decryptedMessage = map (\c -> decrypt c d n) encryptedMessage
    let str = intToStr decryptedMessage 
    let encStr = intToStr encryptedMessage
    print encStr
    print str 

