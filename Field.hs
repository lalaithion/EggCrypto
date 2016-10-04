{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeApplications, DataKinds #-}

module Field where

    import Data.Proxy
    import GHC.TypeLits

    -- This is the extended greatest common demominator algorithm
    gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
    gcdExt a 0 = (1, 0, a)
    gcdExt a b = (t, s - q * t, g)
        where (q, r) = quotRem a b
              (s, t, g) = gcdExt b r

    -- This function returns the multiplicative inverse (1/a) of a number
    -- in the integers modulo n. If n is prime, then 1/a is guaranteed to be
    -- another integer modulus n unless a == 0.
    modMulInv :: Integer -> Integer -> Integer
    modMulInv a n = i
        where (i, _, _) = gcdExt a n

    -- This function checks every odd number and two by calling itself until
    -- the number being checked is greater than n / 2.
    -- TODO: This should be optimized into a check for the sqrt of n, not n / 2
    isPrimeLoop :: Integer -> Integer -> Bool
    isPrimeLoop n i
        | i > (div n 2) = True
        | i == 2 = (mod n i /= 0) && isPrimeLoop n (i+1)
        | otherwise = (mod n i /= 0) && isPrimeLoop n (i+2)

    -- This function calls isPrimeLoop with the initial value of 2.
    isPrime :: Integer -> Bool
    isPrime n = isPrimeLoop n 2

    -- This defined a new type FieldElem
    -- an element of a prime field Z/n where n is prime
    newtype FieldElem (n :: Nat) = FieldElem Integer

    -- This takes a FieldElem and makes it into an Integer
    toInt :: forall n. KnownNat n => FieldElem n -> Integer
    toInt (FieldElem x) = x

    -- This subclasses the Num type, giving FieldElem addition, subtraction
    -- multiplucation, conversion from an integer, absolute value, and sign
    instance KnownNat n => Num (FieldElem n) where
        FieldElem x + FieldElem y = FieldElem (mod (x + y) n)
            where
                n = natVal (Proxy :: Proxy n)
        FieldElem x - FieldElem y = FieldElem (mod (x - y) n)
            where
                n = natVal (Proxy :: Proxy n)
        FieldElem x * FieldElem y = FieldElem (mod (x * y) n)
            where
                n = natVal (Proxy :: Proxy n)
        fromInteger x = FieldElem (mod x n)
            where
                n = natVal (Proxy :: Proxy n)
        abs x = x
        signum x = 1

    -- This provides a way for FieldElems to be printed easily.
    instance KnownNat n => Show (FieldElem n) where
        show (FieldElem x) = show x ++ " mod " ++ show n
            where
                n = natVal (Proxy :: Proxy n)

    -- This provides a way for FieldElems to be compared for equality.
    instance KnownNat n => Eq (FieldElem n) where
        FieldElem x == FieldElem y = x == y

    -- Since division isn't part of the Num class, we create a new Field division
    -- operator using modMulInv
    -- TODO: What's the most useful way to deal with division by zero errors in
    -- Haskell?
    (//) :: forall n. KnownNat n => FieldElem n -> FieldElem n -> FieldElem n
    (//) (FieldElem x) (FieldElem y)
        | y == 0 = error "Division by Zero"
        | otherwise = FieldElem (x * modMulInv y n)
        where
            n = natVal (Proxy :: Proxy n)
