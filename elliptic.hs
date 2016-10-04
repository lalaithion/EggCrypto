{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeApplications, DataKinds #-}

import Data.Bits
import Data.Char

import Field

-- Create a type and constructor so we don't keep having to declare what
-- size field this is, especially as the prime gets huge
type BigField = FieldElem 3851

bigField :: Integer -> FieldElem 3851
bigField x = FieldElem (mod x 3851)

-- This is the type that holds the parameters for the elliptic curve itself
data Curve = Curve {alpha::BigField, beta::BigField} deriving (Eq, Show)

-- This function provides a formatted form for the
formatCurve :: Curve -> String
formatCurve x = "y^2 = x^3 + (" ++ show (alpha x) ++ ")x + ("
    ++ show (beta x) ++ ")"

-- This is the type that holds a point on the curve
data Point = Point {xcoord::BigField, ycoord::BigField} | Ideal
    deriving (Eq, Show)

-- This function tests to see if a Point is on the Curve
testPoint :: Curve -> Point -> Bool
testPoint (Curve a b) Ideal = True
testPoint (Curve a b) (Point x y) = (y^2 == (x^3 + a*x + b))

-- This function takes a point and negates it
negPoint :: Point -> Point
negPoint (Point x y) = Point x (-y)

-- This function takes two points and "adds" them, assuming they are different,
-- non ideal points
simpleAdd :: Curve -> Point -> Point -> Point
simpleAdd c p q = Point x (-y)
    where
        m = (ycoord q - ycoord p) // (xcoord q - xcoord p)
        x = m^2 - xcoord q - xcoord p
        y = m * (x - xcoord p) + ycoord p

-- This function "adds" two points that happen to be the same point
derivAdd :: Curve -> Point -> Point
derivAdd c p
    | ycoord p == 0 = Ideal
    | otherwise = Point x (-y)
    where
        m = (3 * (xcoord p)^2 + alpha c) // (2 * ycoord p)
        x = m*m - 2 * xcoord p
        y = m * (x - xcoord p) + ycoord p

-- This function uses the above to add points in various cases
addPoint :: Curve -> Point -> Point -> Point
addPoint _ x Ideal = x
addPoint _ Ideal x = x
addPoint c p q
    | p == q = derivAdd c p
    | xcoord p == xcoord q = Ideal
    | otherwise = simpleAdd c p q

-- This function implements the loop for multiplication that is more efficient
-- than repeated addition
mulLoop :: Curve -> Integer -> Integer -> Point -> Point -> Point
mulLoop curve index limit acc obj
    | index > limit = acc
    | limit .&. index == index = mulLoop curve j limit r q
    | otherwise = mulLoop curve j limit acc q
    where
        q = (addPoint curve obj obj)
        r = (addPoint curve acc q)
        j = (shift index 1)

-- This function uses the above to multiply a point times and integer
mulPoint :: Curve -> Integer -> Point -> Point
mulPoint _ _ Ideal = Ideal
mulPoint c n p
    | n < 0 = mulPoint c (-n) (negPoint p)
    | n == 0 = Ideal
    | n == 1 = p
    | mod n 2 == 1 = mulLoop c 2 n p p
    | otherwise = mulLoop c 2 n Ideal p

-- This function parses a point out of a string representation of a pair of
-- integers, separated by soem string of non-digits.
parsePoint :: [Char] -> Point
parsePoint str = Point (bigField pint) (bigField qint)
    where
        (_,str1) = break isDigit str
        (p,str2) = span isDigit str1
        (_,str3) = break isDigit str2
        (q,_) = span isDigit str3
        pint = (read p :: Integer)
        qint = (read q :: Integer)

-- This function reduces a point to a pair of Integers
pointPair :: Point -> (Integer,Integer)
pointPair (Point a b) = (toInt a, toInt b)

-- This is the main function which asks for user input
main = do
    putStrLn "## Elliptic Curve Diffie Hellman Key Exchange ##"
    let curve = Curve (bigField 324) (bigField 1287)
    let cmul = mulPoint curve
    let testc = testPoint curve
    --uncomment below to view the elliptic curve we are working on
    --putStrLn $ "curve = " ++ formatCurve curve
    let px = (bigField 920)
    let py = (bigField 303)
    let p = Point px py
    --uncomment below to view the point we start with
    --putStrLn $ "p = " ++ (show $ pointPair p)
    --putStrLn $ "Testing point: " ++ (show $ testc p)
    putStrLn "Enter your secret number"
    my_str <- getLine
    let my_secret = read my_str :: Integer
    let send = cmul my_secret p
    --putStrLn $ "Testing point: " ++ (show $ testc send)
    putStrLn $ "Send this to your partner: "
        ++ (show $ pointPair send)
    putStrLn "Put your partner's point here: "
    partner_point <- getLine
    let final = cmul my_secret (parsePoint partner_point)
    putStrLn $ "Final Shared Secret (Keep Secret) = "
        ++ (show $ pointPair final)
    --putStrLn $ "Testing point: " ++ (show $ testc final)
