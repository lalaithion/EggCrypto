{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeApplications #-}

module Field where

    import Data.Maybe
    import Data.Proxy
    import Data.Ratio
    import GHC.TypeLits



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

    newtype FieldElem (n :: Nat) = FieldElem Integer

    toInt :: forall n. KnownNat n => FieldElem n -> Integer
    toInt (FieldElem x) = x

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

    instance KnownNat n => Show (FieldElem n) where
        show (FieldElem x) = show x ++ "(mod " ++ show n ++ ")"
            where
                n = natVal (Proxy :: Proxy n)

    instance KnownNat n => Read (FieldElem n) where
        readsPrec _ input = [(FieldElem x,"")]
            where
                xstr = takeWhile (/= '(') input
                x = read xstr :: Integer

    instance KnownNat n => Eq (FieldElem n) where
        FieldElem x == FieldElem y = x == y

    (//) :: forall n. KnownNat n => FieldElem n -> FieldElem n -> FieldElem n
    (//) (FieldElem x) (FieldElem y)
        | y == 0 = error "Division by Zero"
        | otherwise = FieldElem (x * modMulInv y n)
        where
            n = natVal (Proxy :: Proxy n)
