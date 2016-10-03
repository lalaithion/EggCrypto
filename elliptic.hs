import Data.Bits
import Data.Ratio

import Field

data Curve = Curve {alpha::Rational, beta::Rational} deriving (Eq, Show, Read)

formatCurve :: Curve -> String
formatCurve x = "y^2 = x^3 + (" ++ show (alpha x) ++ ")x + (" ++ show (beta x) ++ ")"

data Point = Point {xcoord::Rational, ycoord::Rational} | Ideal deriving (Eq, Show, Read)
data Line = Line {point1::Point, point2::Point}

validCurve :: Curve -> Bool
validCurve (Curve a b) = ((-16 * (4 * a^3 + 27 * b^2)) /= 0)

testPoint :: Curve -> Point -> Bool
testPoint (Curve a b) Ideal = True
testPoint (Curve a b) (Point x y) = (y*y == (x^3 + a*x + b))

negPoint :: Point -> Point
negPoint (Point x y) = Point x (-y)

simpleAdd :: Curve -> Point -> Point -> Point
simpleAdd c p q = Point x (-y)
    where
        m = (ycoord q - ycoord p) / (xcoord q - xcoord p)
        x = m^2 - xcoord q - xcoord p
        y = m * (x - xcoord p) + ycoord p

derivAdd :: Curve -> Point -> Point
derivAdd c p
    | ycoord p == 0 = Ideal
    | otherwise = Point x (-y)
    where
        m = (3 * (xcoord p)^2 + alpha c) / (2 * ycoord p)
        x = m^2 - 2 * xcoord p
        y = m * (x - xcoord p) + ycoord p

addPoint :: Curve -> Point -> Point -> Point
addPoint _ x Ideal = x
addPoint _ Ideal x = x
addPoint c p q
    | p == q = derivAdd c p
    | xcoord p == xcoord q = Ideal
    | otherwise = simpleAdd c p q

mulLoop :: Curve -> Int -> Int -> Point -> Point -> Point
mulLoop curve index limit acc obj
    | index > limit = acc
    | (.&.) limit index == index = mulLoop curve j limit r q
    | otherwise = mulLoop curve j limit acc q
    where
        q = (addPoint curve obj obj)
        r = (addPoint curve acc q)
        j = (shift index 1)

mulPoint :: Curve -> Int -> Point -> Point
mulPoint _ _ Ideal = Ideal
mulPoint c n p
    | n < 0 = mulPoint c (-n) (negPoint p)
    | n == 0 = Ideal
    | n == 1 = p
    | mod n 2 == 1 = mulLoop c 2 n p p
    | otherwise = mulLoop c 2 n Ideal p

main = do
    let field11 = newFiniteField 11
    putStrLn "## Testing Field ##"
    let a = (new field11) 5
    putStrLn ("a = " ++ show a)
    let b = (new field11) 4
    putStrLn ("b = " ++ show b)
    let c = (add field11) a b
    putStrLn ("a + b = " ++ show c)
    let d = (add field11) a ((add field11) a b)
    putStrLn ("a + a + b = " ++ show d)

    putStrLn "## Testing Curves ##"
    let curve = Curve (-2) 4
    putStrLn ("curve = " ++ formatCurve curve)
    let p = Point 3 5
    putStrLn ("p = " ++ show p)
    let q = Point (-2) 0
    putStrLn ("q = " ++ show q)
    let cadd = addPoint curve
    let cmul = mulPoint curve
    let ctest = testPoint curve
    let r = cadd q p
    putStrLn ("q + p = " ++ show r ++ " : " ++ show (ctest r))
    let s = cadd q (negPoint q)
    putStrLn ("q - q = " ++ show s ++ " : " ++ show (ctest s))
    let t = cadd p p
    putStrLn ("p + p = " ++ show t ++ " : " ++ show (ctest t))
    let u = cmul 2 p
    putStrLn ("p * 2 = " ++ show u ++ " : " ++ show (ctest u))
    let v = cmul 5 p
    putStrLn ("p * 5 = " ++ show v ++ " : " ++ show (ctest v))
