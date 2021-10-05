module BTree where

import Distribution.Compat.Binary (Binary, bitReverse16)
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) 
            deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf 
insert' b (Node left a right)
   | b == a = Node left a right
   | b < a  = Node (insert' b left) a right 
   | b > a  = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf 
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right) 

testTree' :: BinaryTree Integer 
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
    then print "Yup OK Great!!!"
    else error "Test Failed."

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : y
    where y = preorder left ++ preorder right 

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ n 
     where n = a : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree = undefined 

testTree :: BinaryTree Integer 
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
    if preorder testTree == [2,1,3]
    then putStrLn "Great"
    else putStrLn  "Bad news Bears"

testInorder :: IO ()
testInorder = 
    if inorder testTree == [1,2,3]
    then putStrLn "Great"
    else putStrLn  "Bad news Bears"

testPostorder :: IO ()
testPostorder = 
    if postorder testTree == [1,3,2]
    then putStrLn "Great"
    else putStrLn  "Bad news Bears"

main :: IO ()
main = do 
    testPreorder
    testPostorder
    testInorder

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold = undefined