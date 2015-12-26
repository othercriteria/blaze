module UTree
    where

import Data.List

-- Deprecated: this development didn't work.

-- Crude implementation of a purely functional updatable tree, where
-- each node knows what the tree without a child included and the tree
-- with an additional child node grafted to itself look like.

-- This permits reasonably efficient functional updates of the tree.

data UTree a = UNode { nvalue::a
                     , children::[UTree a]
                     , removeChild::(UTree a -> UTree a)
                     , addChild::(UTree a -> UTree a)        
                     }
             | URoot

isRoot :: UTree a -> Bool
isRoot URoot = True
isRoot _     = False

instance (Eq a) => Eq (UTree a) where
    u1 == u2 = (toTree u1) == (toTree u2)

data Tree a = Node { v::a
                   , c::[Tree a]
                   }
            | Root
            deriving(Show,Eq)

toTree :: UTree a -> Tree a
toTree (UNode v cs _ _) = Node v (map toTree cs)
toTree URoot            = Root

instance (Show a) => Show (UTree a) where
    show = show . toTree

propagateUpdate :: (Eq a) => UTree a -> UTree a -> UTree a
propagateUpdate p c | (isRoot p) = c { removeChild = rcr, addChild = acr }
                    | otherwise  = p { removeChild = rc,  addChild = ac  }
    where rcr u = c `remove` u
          rc  u = (addChild (p `removeChild` c)) (rcr u)
          acr u = c `add` u
          ac  u = (addChild (p `removeChild` c)) (acr u)

remove :: (Eq a) => UTree a -> UTree a -> UTree a
remove u c = u { children = delete c . children $ u }

add :: (Eq a) => UTree a -> UTree a -> UTree a
add p c = p { children = c : (children p) }
          
toUTree :: (Eq a) => UTree a -> Tree a -> UTree a
toUTree p (Node v cs) = new
    where new  = UNode v (map (toUTree new) cs) r a
          new' = propagateUpdate p new
          r    = removeChild new'
          a    = addChild new'

-- Useful operations involving UTrees

newUNode :: (Eq a) => a -> UTree a
newUNode v = toUTree URoot (Node v [])

drill :: [Int] -> UTree a -> UTree a
drill []     u = u
drill (a:as) u = drill as $ children u !! a

insertAt :: (Eq a) => [Int] -> UTree a -> UTree a -> UTree a
insertAt address u v = (drill address u) `addChild` v

removeAt :: (Eq a) => [Int] -> UTree a -> UTree a
removeAt address u = p `removeChild` c
    where p = drill (init address) u
          c = drill [last address] p
                 
-- Minimal tests to show that this works.
                         
test :: UTree Int
test = toUTree URoot $
       Node 3 [Node 2 [Node 1 [], Node 3 [], Node 2 [], Node 2 []], Node 4 []]

test1 :: UTree Int
test1 = drill [0,0] test

test2 :: UTree Int
test2 = drill [0] test

test3 :: UTree Int
test3 = drill [0] test2
        
test4 :: UTree Int
test4 = test2 `remove` test3

test5 :: UTree Int
test5 = (drill [0] ((drill [0] test) `removeChild` (drill [0,1] test)))
        `addChild` (newUNode 1234)

