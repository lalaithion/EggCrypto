module Field where

    import Data.Maybe

    data FiniteField = FiniteField {add::(FieldElement -> FieldElement -> FieldElement),
                  addinv::(FieldElement -> FieldElement),
                  mul::(FieldElement -> FieldElement -> FieldElement),
                  mulinv::(FieldElement -> FieldElement),
                  new::(Integer -> FieldElement)
                  }

    newtype FieldElement = FieldElement Integer deriving (Eq, Show, Read)

    toInt :: FieldElement -> Integer
    toInt (FieldElement x) = x

    gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
    gcdExt a 0 = (1, 0, a)
    gcdExt a b = (t, s - q * t, g)
        where (q, r) = quotRem a b
              (s, t, g) = gcdExt b r

    modMulInv :: Integer -> Integer -> Integer
    modMulInv a n = i
        where (i, _, _) = gcdExt a n

    isPrimeLoop :: Integer -> Integer -> Bool
    isPrimeLoop n i
        | i == n = True
        | i == 2 = (mod n i /= 0) && isPrimeLoop n (i+1)
        | otherwise = (mod n i /= 0) && isPrimeLoop n (i+2)

    isPrime :: Integer -> Bool
    isPrime n = isPrimeLoop n 2

    newFiniteField :: Integer -> FiniteField
    newFiniteField n = FiniteField add addinv mul mulinv new
        where
            add = (\x y -> FieldElement (mod (toInt x + toInt y) n) )
            addinv = (\x -> FieldElement (mod (n - toInt x) n) )
            mul = (\x y -> FieldElement (mod (toInt x * toInt y) n) )
            mulinv = (\x -> FieldElement (mod (modMulInv (toInt x) n) n) )
            new = (\x -> FieldElement x)
