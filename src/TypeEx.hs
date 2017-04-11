module TypeEx 
( Point(..)
, Shape(..)
, area
, Vector(..)
, vplus
, vmult
, dotProd
, Day(..)
, LockerMap
, LockerState(..)
, Code
, Tree(..)
, singleton
,treeInsert
) where

import qualified Data.Map as Map

--derive from other type classes like show!
data Point = Point Float Float deriving (Show)
--value constructors are really just functions accepting n args of type t
--pattern matching relies on constructor patterns at the end of the day
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--type parameters! Take a type as a parameter and produce a new type. Intersting hey? 
--Note that we shouldn't put type constraints into data declarations, even if it seems to make sense.
--Reason for this has to do with any functions which implement the types will need to pass the constraint in anyways, so adding the additional constraint
--just makes things more difficul
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a ->  a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

--enum data types are real easy to derive lots of fun stuff with
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

--what bout type synonyms?
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

--hmm I'm not so good at this part. let's see if typing helps. We're dealing with the Either type class, which can result in different value types based on
--which constructor is used


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
    Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


-- lets build a binary tree using recursive data structures!
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

--inserts a value into a Tree. Base case is singleton. No duplicates btw
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left 
    | x > a= treeElem x right

--Fuck haskell is awesome sometimes
numTree = foldr treeInsert EmptyTree [8,4,7,23,5,3]

-----------------------------------------------------------------------------------------------------
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where 
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

--functors yo!
--instance Functor Maybe where
--    fmap f (Just x) = Just (f x)
--    fmap f Nothing = Nothing
--
instance Functor Tree where 
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
