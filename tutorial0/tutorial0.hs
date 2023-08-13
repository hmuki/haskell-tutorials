-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 0
--
-- Week 0(17-21 Sep.)
--
-- Insert your name and matriculation number here:
-- Name:
-- Nr. :


import Test.QuickCheck

-- Exercise 3:

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- Exercise 4:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = (c^2==a^2+b^2)||(a^2==b^2+c^2)||(b^2==a^2+c^2)


-- Exercise 5:

leg1 :: Int -> Int -> Int
leg1 x y = x^2 - y^2

leg2 :: Int -> Int -> Int
leg2 x y = 2 * y * x

hyp :: Int -> Int -> Int
hyp x y = x^2 + y^2

-- Exercise 6:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)
