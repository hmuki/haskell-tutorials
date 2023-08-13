-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 8(05-09 Nov.)

module Tutorial7 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen list = maximum [length xs | xs <- map fst $ map snd list]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (bcode,item)
 | n >= length (fst item) = bcode ++ "..." ++ fst item ++ "..." ++ snd item ++ "\n"
 | otherwise = "Not_found!"

showCatalogue :: Catalogue -> String
showCatalogue dbase = concat [formatLine (length $ fst $ snd s) s | s <- toList dbase]

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe s = Just (head s)

catMaybes :: [Maybe a] -> [a]
catMaybes xs = concat [ maybeToList s | s <- xs]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bcodes dbase = catMaybes [ get s dbase | s <- bcodes]


-- Exercise 4
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 12

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
