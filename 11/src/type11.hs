--module type11
module Type11 where

import Data.Char

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType
myHusky :: HuskyType a
myHusky = HuskyData
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData
myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10


--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10

data Doggies a = Husky a | Mastiff a
  deriving (Eq, Show)

data Price = 
  Price Integer deriving (Eq, Show)


data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data PlaneSize = PlaneSize Integer
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline PlaneSize
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (PlaneSize 10)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False 

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m



-- ARITY page 399

-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)
-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)
-- product of Int and String
data Example2 =
  Example2 Int String
  deriving (Eq, Show)

-- NewType (AnyVal)
newtype Goats =
  Goats Int deriving (Eq, Show)
newtype Cows =
  Cows Int deriving (Eq, Show) 

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n ) = n > 42


class TooMany a where
  tooMany :: a -> Bool 

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

data QuantumBool = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

--type alias creates type constructor, but not data constructors (scala: type)
type TwoQsAsTuple = (QuantumBool, QuantumBool) 

--record syntax
data Person = 
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving Show
type AuthorName = String
data Author = Author (AuthorName, BookType)
--normal Form
data NAuthor = 
    NFiction AuthorName
  | NNonFiction AuthorName
  deriving (Eq, Show)

data Expr = 
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- equivalent to:
--type Number = Int
--type Add = (Expr, Expr)
--type Minus = Expr
--type Mult = (Expr, Expr)
--type Divide = (Expr, Expr)
--type Expr =
--  Either Number
--    (Either Add
--      (Either Minus
--        (Either Mult Divide)))

data GuessWhat =
  ChickenButt deriving (Eq, Show)
data Id a =
  MkId a deriving (Eq, Show)
data Product a b =
  Product a b deriving (Eq, Show)
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)
data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)



newtype NumCow =
  NumCow Int
  deriving (Eq, Show)
newtype NumPig =
  NumPig Int
  deriving (Eq, Show)
data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

--Binary Tree
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


btins :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
btins b Leaf = Node Leaf b Leaf
btins b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (btins b left) a right
  | b > a = Node left a (btins b right)


--map over a tree
mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
   Node (mapTree f left) (f a) (mapTree f right)  

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = 
   (a : (preorder left)) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = 
   (inorder left) ++ ( a : (inorder right))


postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = 
   (postorder left) ++ (postorder right) ++ [a]

testTree = (Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node (Node Leaf 4 Leaf) 5 (Node (Node Leaf 6 Leaf) 7 (Node Leaf 8 Leaf))))

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree _ z Leaf = z
foldTree f z (Node left v right) =
  foldTree f (foldTree f (f v z) left) right   

--exercises
--1 -a data constructor
--2 -c Weekday -> String
--3 -b Capital Letter
--4 -c last element (or blow).


     
   
