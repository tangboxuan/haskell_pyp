import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table and appears only once
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xs
  = fromJust (lookup x xs)

--Pre: xs is defined for each variable in the BDD
checkSat :: BDD -> Env -> Bool
checkSat (root, nodes) xs
  = checkSat' root
    where
      checkSat' 1 = True
      checkSat' 0 = False
      checkSat' i
        | v         = checkSat' r
        | otherwise = checkSat' l
        where
          (x, l, r) = lookUp i nodes
          v         = lookUp x xs

sat :: BDD -> [[(Index, Bool)]]
sat (root, nodes)
  = sat' root
    where
      sat' 1 = [[]]
      sat' 0 = []
      sat' i
        = [ (x, False):y | y <- sat' l ] ++ [ (x, True):y | y <- sat' r ]
        where
          (x, l, r) = lookUp i nodes

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim x))
  = Prim (not x)
simplify (Or (Prim x) (Prim y))
  = Prim (x || y)
simplify (And (Prim x) (Prim y))
  = Prim (x && y)
simplify a
  = a

restrict :: BExp -> Index -> Bool -> BExp
restrict exp i v
  = restrict' exp
  where
    restrict' a@(IdRef x)
      | x == i    = Prim v
      | otherwise = a
    restrict' (And a b)
      = simplify (And (restrict' a) (restrict' b))
    restrict' (Or a b)
      = simplify (Or (restrict' a) (restrict' b))
    restrict' (Not a)
      = simplify (Not (restrict' a))
    restrict' a
      = a

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs
  = buildBDD' e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e n (x:xs)
  = (n, [(n, (x, l', r'))] ++ ln ++ rn)
  where
    l        = 2 * n
    r        = 2 * n + 1
    (l', ln) = buildBDD' (restrict e x False) l xs
    (r', rn) = buildBDD' (restrict e x True) r xs
buildBDD' (Prim v) n []
  | v         = (1, [])
  | otherwise = (0, [])

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])