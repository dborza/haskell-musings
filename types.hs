-- Just do some basic testing with types and constructors and stuff

data Point = Point Float Float deriving (Show)
-- Value constructors are functions
data Shape = Circle Point Float 
			| Rectangle Point Point
			| Triangle Point Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
surface (Triangle _ _ _) = -100	-- instead of the standard function we just threw a value here.

-- record syntax, first variant
-- fist way of definig a person
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor  

-- second (and more succint) way of defining a person
-- record syntax, second variant
data Cat = Cat {
	size :: String,
	legs :: Int,
	race :: String
} deriving (Show)

--	type parameters (something like "Generics" in Java?)
-- 	type parameters are declared before the "=" sign in the data declaration
data Maybe a = Nothing | Just a

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a b c) `vplus` (Vector x y z) = (Vector (a + x) (b + y) (c + z))

-- something like an enum
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- recursive datastructures
data MyList a = Empty | Cucu a (MyList a) deriving (Show, Read, Eq, Ord)

--	Definition of a Binary Tree
data BinaryTree a = EmptyTree | TreeNode a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq, Ord)

singletonTree :: a -> BinaryTree a
singletonTree a = TreeNode a EmptyTree EmptyTree

--	Insert a new element in a Binary Search Tree
treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (TreeNode y leftTree rightTree)
	| (x == y) = TreeNode x leftTree rightTree
	| (x < y) = TreeNode y (treeInsert x leftTree) rightTree
	| (x > y) = TreeNode y leftTree (treeInsert x rightTree)
	
-- Determine if a given element is contained in a given tree
treeElem :: (Ord a) => a -> BinaryTree a -> Bool
treeElem x EmptyTree = False
treeElem x (TreeNode a leftTree rightTree) 
	| (x == a) = True
	| (x < a) = treeElem x leftTree
	| (x > a) = treeElem x rightTree
	
--	typeclasses - they're (almost) like interfaces
-- when we say that a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with that type	
main = print $ treeElem 10 $ foldr treeInsert EmptyTree [1,2,3,4,5,6,7,8]