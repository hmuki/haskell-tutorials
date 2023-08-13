-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 10(19-23 Nov.)
module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck
import Test.QuickCheck.Modifiers

type Row a     =  [a]
type Col a     =  [a]
type Matrix a  =  Col (Row a)
type Digit     =  Char

digits :: [Digit]
digits =  ['1'..'9']

blank :: Digit -> Bool
blank d  =  d == ' '

-- 2.
group :: [a] -> [[a]]
group ns = groupBy 3 ns

groupBy :: Int -> [a] -> [[a]]
groupBy k ns = [ take k (drop (i*k) ns) | i <- [0..stop]]
 where
  stop = (length ns `div` k) - 1

-- 3.
intersperse :: a -> [a] -> [a]
intersperse c [] = [c]
intersperse c (str:rest) = c : str : intersperse c rest 

-- 4.
showRow :: String -> String
showRow str = concat $ intersperse "|" (group str)

-- 5.
showMat :: [String] -> [String]
showMat xs = [unlines $ concat $ intersperse [replicate (length $ head xs) '-'] (group xs)]

-- 6.
put :: Matrix Digit -> IO ()
put xs = putStr $ concat $ showMat xs

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices xss = [[ if (blank x) then ['1'..'9']
                else [x] | x <- xs] | xs <- xss]

cp :: [[a]] -> [[a]]
cp []        =  [[]]
cp (xs:xss)  =  [ x:ys | x <- xs, ys <- cp xss ]

-- 8.
expand :: Matrix [Digit] -> [Matrix Digit]
expand xss = [ [pushIn ',' (length xss) x] | x <- cp $ concat xss ]

pushIn :: a -> Int -> [a] -> [a]
pushIn c n xs
 | length xs <= n = xs
 | otherwise = take n xs ++ [c] ++ pushIn c n (drop n xs)

-- 11, 12, 13.
-- transpose :: [[a]] -> [[a]]
-- transpose [xs]      =  [[x] | x <- xs]
-- transpose (xs:xss)  =  zipWith (:) xs (transpose xss)

--prop :: Int -> Bool
--prop x = x+x == 2*x

rows, cols, boxs :: Matrix a -> Matrix a
rows xss = xss
cols xss = transpose xss
boxs = map concat . concat . map cols . group . map group

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs  = nub xs == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g  = and [ distinct xs | xs <- g]
           &&
	   and [ distinct xs | xs <- cols g]
	   &&
	   and [ distinct xs | xs <- boxs g]
-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple =  filter valid . expand . choices

isMatrix :: Matrix Digit -> Bool
isMatrix xss = length xss == 9 && and [ length xs == 9 | xs <- xss ]

prop_involutions :: Matrix Digit -> Bool
prop_involutions xss | any (\x -> x == "") xss = True
		     | not (isMatrix xss) = True
		     | otherwise = (rows.rows) xss == xss
                          &&
		         (cols.cols) xss == xss
		          &&
		         (boxs.boxs) xss == xss
-- 18.
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [if (ok y) then y else (y \\ concat (singles)) | y <- row ]
 where
  singles = [ x | x <- row, length x == 1]
  ok y = length y <= 1

-- 19.
pruneBy :: (Matrix [Digit] -> Matrix [Digit])
             -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune xss = pruneBy rows (pruneBy cols (pruneBy boxs xss))

-- 20.
close :: (Eq a,Ord a) => [(a,a)] -> [(a,a)]
close pairs = nub (sort (pairs ++
                         [ (x,z) | (x,y) <- pairs,
			   (y',z) <- pairs,
			    y == y']))
			    
many :: Eq a => (a -> a) -> a -> a
many f x
 | f x == x = x
 | otherwise = many f (f x)

-- 21.
the :: [Digit] -> Digit
the [d]  =  d

extract :: Matrix [Digit] -> Matrix Digit
extract xss = [[ the x | x <- xs ] | xs <- xss]

-- 22.
solve :: Matrix Digit -> Matrix Digit
solve xss = extract (many prune (choices xss))

-- 23.
failed :: Matrix [Digit] -> Bool
failed xss = and [ and [ x == "" | x <- xs ] | xs <- xss ]
       

-- 24.
solved :: Matrix [Digit] -> Bool
solved xss = and [ and [ length x == 1 | x <- xs ] | xs <- xss ]

-- 25.
smallest :: Matrix [Digit]
smallest = undefined

-- 27.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = undefined 

-- 28.
search :: Matrix Digit -> [Matrix Digit]
search = undefined


-- Example from Bird

book    :: Matrix Digit
book    =  ["  4  57  ",
            "     94  ",
            "36      8",
            "72  6    ",
            "   4 2   ",
            "    8  93",
            "4      56",
            "  53     ",
            "  61  9  "]

-- Examples from websudoku.com

easy    :: Matrix Digit
easy    =  ["    345  ",
            "  89   3 ",
            "3    2789",
            "2 4  6815",
            "    4    ",
            "8765  4 2",
            "7523    6",
            " 1   79  ",
            "  942    "]

medium  :: Matrix Digit
medium  =  ["   4 6 9 ",
            "     3  5",
            "45     86",
            "6 2 74  1",
            "    9    ",
            "9  56 7 8",
            "71     64",
            "3  6     ",
            " 6 9 2   "]

hard    :: Matrix Digit
hard    =  ["9 3  42  ",
            "4 65     ",
            "  28     ",
            "     5  4",
            " 67 4 92 ",
            "1  9     ",
            "     87  ",
            "     94 3",
            "  83  6 1"]

evil    :: Matrix Digit
evil    =  ["  9      ",
            "384   5  ",
            "    4 3  ",
            "   1  27 ",
            "2  3 4  5",
            " 48  6   ",
            "  6 1    ",
            "  7   629",
            "     5   "]
br :: IO ()
br = putStrLn "***"

puts :: [Matrix Digit] -> IO ()
puts  =  sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g  =  put g >>
             puts (search g) >>
             br
       
main =  puzzle easy >>
        puzzle medium >>
        puzzle hard >>
        puzzle evil

