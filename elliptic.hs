{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeApplications, DataKinds #-}

import Data.Bits
import Data.Ratio
import Data.Proxy
import Data.Maybe
import GHC.TypeLits

import Field

type BigField = FieldElem 3851

bigField :: Integer -> FieldElem 3851
bigField x = FieldElem x

data Curve = Curve {alpha::BigField, beta::BigField} deriving (Eq, Show)

formatCurve :: Curve -> String
formatCurve x = "y^2 = x^3 + (" ++ show (alpha x) ++ ")x + (" ++ show (beta x) ++ ")"

data Point = Point {xcoord::BigField, ycoord::BigField} | Ideal deriving (Eq, Show, Read)
data Line = Line {point1::Point, point2::Point}

testPoint :: Curve -> Point -> Bool
testPoint (Curve a b) Ideal = True
testPoint (Curve a b) (Point x y) = (y*y == (x*x*x + a*x + b))

negPoint :: Point -> Point
negPoint (Point x y) = Point x (-y)

simpleAdd :: Curve -> Point -> Point -> Point
simpleAdd c p q = Point x (-y)
    where
        m = (ycoord q - ycoord p) // (xcoord q - xcoord p)
        x = m*m - xcoord q - xcoord p
        y = m * (x - xcoord p) + ycoord p

derivAdd :: Curve -> Point -> Point
derivAdd c p
    | ycoord p == 0 = Ideal
    | otherwise = Point x (-y)
    where
        m = (3 * (xcoord p)*(xcoord p) + alpha c) // (2 * ycoord p)
        x = m*m - 2 * xcoord p
        y = m * (x - xcoord p) + ycoord p

addPoint :: Curve -> Point -> Point -> Point
addPoint _ x Ideal = x
addPoint _ Ideal x = x
addPoint c p q
    | p == q = derivAdd c p
    | xcoord p == xcoord q = Ideal
    | otherwise = simpleAdd c p q

mulLoop :: Curve -> Integer -> Integer -> Point -> Point -> Point
mulLoop curve index limit acc obj
    | index > limit = acc
    | limit .&. index == index = mulLoop curve j limit r q
    | otherwise = mulLoop curve j limit acc q
    where
        q = (addPoint curve obj obj)
        r = (addPoint curve acc q)
        j = (shift index 1)

mulPoint :: Curve -> Integer -> Point -> Point
mulPoint _ _ Ideal = Ideal
mulPoint c n p
    | n < 0 = mulPoint c (-n) (negPoint p)
    | n == 0 = Ideal
    | n == 1 = p
    | mod n 2 == 1 = mulLoop c 2 n p p
    | otherwise = mulLoop c 2 n Ideal p

main = do
    putStrLn "## Testing Curves ##"
    let curve = Curve (bigField 324) (bigField 1287)
    let cmul = mulPoint curve
    putStrLn ("curve = " ++ formatCurve curve)
    let p = Point (bigField 920) (bigField 303)
    putStrLn ("p = " ++ show p)
    putStrLn ("Enter your secret number")
    my_secret <- getLine
    putStrLn ("Send this to your partner: " ++ show (cmul (read my_secret ::Integer) p))
    putStrLn "Put your partner's point here"
    partner_point <- getLine
    putStrLn ("Final Shared Secret (Keep Secret) = " ++ show (cmul (read my_secret ::Integer) (read partner_point :: Point) ) )
