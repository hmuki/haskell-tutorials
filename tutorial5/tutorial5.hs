-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 6(22-26 Oct.)
module Tutorial5 where

import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )


--------------------------------------------------
--------------------------------------------------
---Implementing propositional logic in Haskell----
--------------------------------------------------
--------------------------------------------------

-- The datatype 'Wff'

type Name = String
data Wff = Var Name
          | F
          | T
          | Not Wff
          | Wff :|: Wff
	  | Wff :->: Wff
	  | Wff :<->: Wff
          | Wff :&: Wff
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Wffs

-- turns a Wff into a string approximating mathematical notation
showWff :: Wff -> String
showWff (Var x)        =  x
showWff (F)            =  "F"
showWff (T)            =  "T"
showWff (Not p)        =  "(~" ++ showWff p ++ ")"
showWff (p :|: q)      =  "(" ++ showWff p ++ "|" ++ showWff q ++ ")"
showWff (p :&: q)      =  "(" ++ showWff p ++ "&" ++ showWff q ++ ")"
showWff (p :->: q)     =  "(" ++ showWff p ++ "->" ++ showWff q ++ ")"
showWff (p :<->: q)    =  "(" ++ showWff p ++ "<->" ++ showWff q ++ ")"
-- evaluates a wff in a given environment
eval :: Env -> Wff -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  eval e p <= eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a wff - 
--  NOTE: variable names in the result must be unique
names :: Wff -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Wff -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


--------------------------------------------------
--------------------------------------------------
------------------ Exercises ---------------------
--------------------------------------------------
--------------------------------------------------

-- 1.
wff1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
wff2 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (((Not (Var "P")) :|: (Not (Var "Q"))) :&: ((Not (Var "P")) :|: (Not (Var "R"))))
wff0 = Var "P" :|: Not (Var "P")


-- 2. 
tautology :: Wff -> Bool
tautology wff
 | satisfiable (Not wff) == False = True
 | otherwise                      = False
 
prop_taut1 :: Wff -> Bool
prop_taut1 wff = tautology wff || satisfiable (Not wff)

prop_taut2 :: Wff -> Bool
prop_taut2 wff = not (satisfiable wff) || not (tautology (Not wff)) 


-- 3.
wff3 = (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
wff4 = ((Var "P" :<->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))) :|: (Not (Var "P") :&: Var "Q")

-- 4.
equivalent :: Wff -> Wff -> Bool
equivalent p q = and [eval e p == eval e q | e <- envs ((nub (names p ++ names q)))]
 


-- 5.
subformulas :: Wff -> [Wff]
subformulas (Var x) = [Var x]
subformulas T = [T]
subformulas F = [F]
subformulas (Not p) = [Not p] ++ subformulas p
subformulas (p :&: q) = [p :&: q] ++ nub (subformulas p ++ subformulas q)
subformulas (p :|: q) = [p :|: q] ++ nub (subformulas p ++ subformulas q)
subformulas (p :->: q) = [p :->: q] ++ nub (subformulas p ++ subformulas q)
subformulas (p :<->: q) = [p :<->: q] ++ nub (subformulas p ++ subformulas q)


--------------------------------------------------
--------------------------------------------------
---------------- Tutorial Activities -------------
--------------------------------------------------
--------------------------------------------------
-- Warmup exercises

---The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int
	   deriving (Eq, Ord)

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 6.

fType :: Fruit -> String
fType (Apple v b) = "Apple"
fType (Orange v n) = "Orange"

fVariety :: Fruit -> String
fVariety (Apple variety b) = variety
fVariety (Orange variety n) = variety

isBloodOrange :: Fruit -> Bool
isBloodOrange f
 | fType f == "Apple" = False
 | fType f == "Orange" = (fVariety f == "Tarocco")||(fVariety f == "Moro")||(fVariety f == "Sanguinello")

-- 7.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments [] = 0
bloodOrangeSegments (f:fs)
 | isBloodOrange f = 1 + bloodOrangeSegments fs
 | otherwise = bloodOrangeSegments fs
-- 8.
containsWorms :: Fruit -> Bool
containsWorms (Apple _ b) = b == True
containsWorms (Orange _ n) = False

worms :: [Fruit] -> Int
worms [] = 0
worms (f:fs)
 | containsWorms f = 1 + worms fs
 | otherwise = worms fs

-- Test your Logic
-- 9.
wff5 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q")) 

wff6 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q") 

equivalent' :: Wff -> Wff -> Bool
equivalent' p q = let w = p :<->: q in tautology w

prop_equivalent :: Wff -> Wff -> Bool
prop_equivalent p q = equivalent p q == equivalent' p q

--------------------------------------------------
--------------------------------------------------
-- Optional Material
--------------------------------------------------
--------------------------------------------------

-- 10.
-- check for negation normal form

isNNF :: Wff -> Bool
isNNF T = True
isNNF F = True
isNNF (Var _) = True
isNNF (_:&:_) = True
isNNF (_:|:_) = True
isNNF (Not (Var _)) = True
isNNF (_:->:_) = True

-- 11.

-- checks for negative normal form

toNNF :: Wff -> Wff
toNNF = undefined

{-NNF :: Wff ->  
NNF (Not (Var "P" :&: Var "Q")) = (Not (Var "P") :|: Not (Var "Q"))
NNF (Not (Var "P" :|: Var "Q")) = (Not (Var "P") :&: Not (Var "Q"))
NNF (Var "P" :->: Var "Q") = (Not (Var "P")) :|: Var "Q"
NNF (Var "P" :<->: Var "Q") = (Var "P" :->: Var "Q") :&: (Var "Q" :->: Var "P")
NNF (Not (Not (Var "P"))) = Var "P"
NNF Var x = Var x
-}


-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff -> Bool
prop_NNF1 f  =  isNNF (toNNF f)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Wff -> Bool
prop_NNF2 f  =  equivalent f (toNNF f)


-- 12.
-- check whether a formula is in conj. normal form
isCNF :: Wff -> Bool
isCNF = undefined


-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Wff]] -> Wff
listsToCNF = undefined


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Wff -> [[Wff]]
listsFromCNF = undefined


-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Wff -> [[Wff]]
toCNFList = undefined



-- convert to conjunctive normal form
toCNF :: Wff -> Wff
toCNF wff  =  listsToCNF (toCNFList wff)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Wff -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Wff where
    show  =  showWff

instance Arbitrary Wff where
    arbitrary  =  sized wff
        where
          wff n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  wff (n `div` 2)
                   subform' =  wff (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Wff] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showWff p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showWff p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Wff -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Wff -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
