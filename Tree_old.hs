module Tree
    where
      
import Control.Monad
import Data.List
import Text.Printf
    
type Address = [Int]
type Tag = String

data Tree a = Node { tvalue::a, children::[Tree a], tagged::Tag }
              deriving (Eq)

isTagged :: Tree a -> Bool
isTagged (Node _ _ "") = False
isTagged (Node _ _ _ ) = True
                      
instance (Show a) => Show (Tree a) where
    show (Node v cs "" ) = printf "(%s: %s)" (show v) $
                           (intercalate "," (map show cs))
    show (Node v cs tag) = (printf "{%s}" tag) ++ (show (Node v cs ""))

-- Basic operations for building trees and making local changes

mkNode :: a -> Tree a
mkNode v = Node v [] ""

tagNode :: Tree a -> Tag -> Tree a
tagNode (Node v cs _) tag = Node v cs tag

mkTagNode :: Tag -> a -> Tree a
mkTagNode tag v = (mkNode v) `tagNode` tag

addChild :: Tree a -> Tree a -> Tree a
addChild t c = t { children = (c:(children t)) }
                         
removeChild :: (Eq a) => Tree a -> Tree a -> Tree a
removeChild t c = t { children = (delete c $ children t) }

-- Operations for making non-local changes to a tree

getAt :: Address -> Tree a -> Tree a
getAt []     t = t
getAt (a:as) t = getAt as (children t !! a)

splitTagged :: [Tree a] -> ([Tree a], [Tree a])
splitTagged = partition isTagged
                 
splitChildren :: Tree a -> ([Tree a], [Tree a])
splitChildren = splitTagged . children
                 
taggedChildren :: Tree a -> [Tree a]
taggedChildren = fst . splitChildren

untaggedChildren :: Tree a -> [Tree a]
untaggedChildren = snd . splitChildren

getTagged :: Tag -> [Tree a] -> Tree a
getTagged tag ts = head . filter ((== tag) . tagged) $ ts

untaggedChild :: Tree a -> Tree a
untaggedChild = head . untaggedChildren
                   
getTagAt :: (Eq a) => Tag -> Address -> Tree a -> Tree a
getTagAt tag a = getTagged tag . children . getAt a

update :: (Eq a) => Tree a -> Address -> Tree a -> Tree a -> Tree a
update t []     t' t'' = (t `removeChild` t') `addChild` t''
update t (a:as) t' t'' = (t `removeChild` c') `addChild` (update c' as t' t'')
    where c' = children t !! a

addAt :: (Eq a) => Tree a -> Address -> Tree a -> Tree a
addAt t []     c = t `addChild` c
addAt t (a:as) c = (t `removeChild` c') `addChild` (addAt c' as c)
    where c' = children t !! a

removeAt :: (Eq a) => Tree a -> Address -> Tree a
removeAt t (a:[]) = t { children = removeA a (children t) }
    where removeA a cs = take a cs ++ drop (a+1) cs
removeAt t (a:as) = (t `removeChild` c') `addChild` (removeAt c' as)
    where c' = children t !! a

-- Determines the (relative) address of a node given a strategy for
-- reaching it.
address :: Tree a -> [Tree a -> Bool] -> Address
address t ps = address' t ps 0
               
address' :: Tree a -> [Tree a -> Bool] -> Int -> Address
address' _                   []     _ = []
address' (Node _ []     _  ) _      _ = error "address': no such children"
address' (Node v (c:cs) tag) (p:ps) i | (p c)     = i : (address' c ps 0)
                                      | otherwise =
                                          address' (Node v cs tag) (p:ps) (i+1)

type TagAddress = [Tag]
                                                   
tagsToAddress :: Tree a -> TagAddress -> Address
tagsToAddress t tags = address t (map (\x -> (== x) . tagged) tags)
                       
-- I think any method of this sort is doomed to failure, because the
-- destination address is relative to the old tree, not the new one
-- created by the removeAt.
-- moveNode :: (Eq a) => Tree a -> Address -> Address -> Tree a
-- moveNode t a1 a2 = addAt (removeAt t a1) a2 (getAt a1 t)

-- Experimenting to see if some sort of monadic structure can be used
-- to handle trees.

-- I think this will be most useful when I need to start transforming
-- trees.

-- Inspired by Sebastian Fischer's Control.Monad.SearchTree

instance Monad Tree where
    return v = Node v [] ""

    (Node v [] t) >>= f = (f v) { tagged = t }
    (Node v cs t) >>= f = Node (tvalue (f v)) (map (\c -> c >>= f) cs) t

-- Crude but effective way of showing the topology of a tree more
-- compactly.
simplify :: Tree a -> Tree String                          
simplify t = t >>= (const $ mkNode "*")
                        
{-                         
Are Monad Laws satisfied?

1. return a >>= f  =  f a:
   return a >>= f -->
   Node a [] >>= f -->
   Node (tvalue (f a)) (map (\c -> c >>= f) []) -->
   Node (tvalue (f a)) [] -->
   f a

   return a >>= f -->
   Node a [] "" >>= f -->
   Node (tvalue (f a)) (map (\x -> c >>= f) []) t -->
   Node (tvalue (f a)) [] t -->
   

   (This law forces the definition of (Node v [] _) >>= f.)

2. m >>= return  =  m:

   if m = (Node v []) then
   m >>= return -->
   (Node v []) >>= return -->
   return v -->
   Node v [] -->
   m

   if m = (Node v cs) then
   m >>= return -->
   (Node v cs as) >>= return -->
   Node (tvalue (return v)) (map (\x -> x >>= return) cs) -->
   Node v (map (\x -> x) cs) -->
   Node v cs -->
   m

   (Assuming that we can make the induction work properly. Do it on
   the shape of the tree? Then we don't necessarily have the monad
   laws for infinite structures? But that doesn't matter. So it will
   be phrased "suppose that Monad Law 2 holds for all trees of depth
   k-1 and let T be a tree of depth k".)


3. (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)
   if m = (Node v []) then
   (m >>= f) >>= g -->
   (Node v [] >>= f) >>= g -->
   (f v) >>= g -->
     if (f v) = Node v' [] then
       (Node v' []) >>= g -->
       g v' -->
       (Node v []) >>= (\x -> f x >>= g) -->
       m >>= (\x -> f x >>= g)
     if (f v) = Node v' cs' then
       (Node v' cs') >>= g -->
       Node (tvalue (g v')) (map (\x -> x >>= g) cs') -->
       Node (tvalue (g (tvalue (f v)))) (map


   m >>= (\x -> f x >>= g) -->
   Two cases:
   m = (Node v []) 
   ((Node v []) >>= f) >>= g -->
   (f v) >>= g -->
   Node 

   (MTerm v []) >>= (\x -> f x >>= g)  or
     (Node v cs) >>= (\x -> f x >>= g) -->
   (\x -> f x >>= g) v  or
     Node (tvalue ((\x -> f x >>= g) v))
           (map (\c -> c >>= (\x -> f x >>= g)) cs) -->
   f v >>= g  or
     Node (tvalue (f v >>= g)) (map (\c -> (c >>= f) >>= g) cs)
   ((MTerm v) >>= f) >>= g  or  ((Node v cs) >>= f) >>= g

   (Sloppiest of the three proofs, but it is suggestive of working.)
   
Probably?
-}


-- Trying the structure out with integer values
{- Examples:
test :: Tree Int
test = Node 3 [Node 2 [Node 1 [], Node 3 [], Node 2 [], Node 2 []], Node 4 []]
==> (3: (2: (1: ),(3: ),(2: ),(2: )),(4: ))

test' :: Tree Int
test = mkNode 3 `addChild` (mkNode 1 `tagNode` "asdf")
==> (3: {asdf}(1: ))

getAt test [] ==> (3: (2: (1: ),(3: ),(2: ),(2: )),(4: ))
getAt test [0] ==> (2: (1: ),(3: ),(2: ),(2: ))
getAt test [0,1] ==> (3: )

getTagAt test' "asdf" [] ==> {asdf}(1: )
getTagAt test' "asdfg" [] ==> Exception

-- updates can affect order
-- so never assume that old addresses are correct
update test [0] (mkNode 3) (mkNode 100)
==> (3: (2: (100: ),(1: ),(2: ),(2: )),(4: ))

addAt test [] (mkNode 5) ==> (3: (5: ),(2: (1: ),(3: ),(2: ),(2: )),(4: ))
addAt test [0,1] (mkNode 5) ==> (3: (2: (3: (5: )),(1: ),(2: ),(2: )),(4: ))

removeAt test [] ==> Exception
removeAt test [0] ==> (3: (4: ))

moveNode test [1] [0,2] ==> (3: (2: (2: (4: )),(1: ),(3: ),(2: )))
-}

-- Monadic expansion tests

{-
test :: Tree Int
test = Node 3
       [Node 2
        [Node 1 [] "a", Node 3 [] "", Node 2 [] "", Node 2 [] ""] "",
        Node 4
        [] "b"] ""

test' :: Tree String
test' = test >>= (\_ -> Node "asdf" [Node "A" [] "", Node "B" [] ""] "5")

test'' :: Tree String
test'' = do
  v  <- Node 100 [] "v"
  v' <- Node 200 [] "v'"
  Node "asdf" [Node (show v) [] "", Node (show v') [] ""] "5"
-}