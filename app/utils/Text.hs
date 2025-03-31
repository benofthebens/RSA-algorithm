module Utils.Text where
import Data.Char

-- Define a function that takes in a string and 
-- turns it into an Int array
strToIntList :: String -> [Integer]
strToIntList = map(toInteger . ord)

-- Define a function that maps an integer list to a String
intListToStr :: [Integer] -> String 
intListToStr intList = map(chr . fromIntegral) intList
