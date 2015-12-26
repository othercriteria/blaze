module Tree
    where
      
import Data.List
import Text.Printf

-- Meaningful names for structures that specify physical and logical
-- addresses for nodes.
type Address = [Int]
type Tag = String
type TagAddress = [Tag]

data Tree a = Node { tvalue::a, children::[Tree a], tagged::Tag }
              deriving (Eq)

instance (Show a) => Show (Tree a) where
    show (Node v cs "" ) = printf "(%s: %s)" (show v)
                           (intercalate "," (map show cs))
    show (Node v cs tag) = printf "{%s}%s" tag (show (Node v cs ""))

isTagged :: Tree a -> Bool
isTagged (Node _ _ "") = False
isTagged (Node _ _ _ ) = True

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

getTagged :: Tag -> [Tree a] -> Tree a
getTagged tag ts = head . filter ((== tag) . tagged) $ ts

untaggedChild :: Tree a -> Tree a
untaggedChild = getTagged "" . children
                   
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
                                                   
tagsToAddress :: Tree a -> TagAddress -> Address
tagsToAddress t tags = address t (map (\x -> (== x) . tagged) tags)
                       
-- I think any method of this sort is doomed to failure, because the
-- destination address is relative to the old tree, not the new one
-- created by the removeAt.
-- moveNode :: (Eq a) => Tree a -> Address -> Address -> Tree a
-- moveNode t a1 a2 = addAt (removeAt t a1) a2 (getAt a1 t)

{- Examples:

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
