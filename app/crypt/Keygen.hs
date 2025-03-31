module Crypt.Keygen (gen) where
import Utils.Maths;

gen :: Integer -> Integer -> IO ((Integer, Integer), (Integer, Integer))
gen from to = do 
    p <- getRandomPrime from to
    q <- getRandomPrime from to

    let modules = p * q;

    publicExponent <- getRandomCoprimeOf p q from to 

    let privateExponent = case invMod publicExponent ((p-1)*(q-1)) of
            Just inv' -> inv'
            Nothing -> error "There is an error"

    return ((publicExponent, modules), (privateExponent, modules))

