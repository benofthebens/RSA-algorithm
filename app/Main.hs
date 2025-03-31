module Main where
import Crypt.Keygen (gen)
    
encrypt :: Integer -> Integer -> Integer -> Integer
encrypt m e n =
    (m ^ e) `mod` n

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt c d n = 
    (c ^ d) `mod` n
    
    
main :: IO ()
main = do 
    keys <- gen 1 1024
    print keys
