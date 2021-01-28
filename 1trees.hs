module Trees where

data Arboles a = Hoj a
             | Void
             | Nod a (Arboles a) (Arboles a)
             deriving (Show, Eq)

--data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

--instance Show a => Show (BTree a) where show t = showSubL t ""

nNodes:: Arboles a -> Int
nNodes Void = 0 
nNodes (Nod _ a b) = nNodes a + nNodes b + 1

nLeaves :: Arboles a -> Int  
nLeaves Void = 0
nLeaves (Nod _ a b) = nNodes a + nNodes b

nni :: Arboles a -> Int  
nni Void = 0
nni (Nod _ a b) = nNodes a + nNodes b

containsU :: (Eq a) => a -> Arboles a -> Bool
containsU p Void = False
containsU elemBuscado (Nod a b c )
          | elemBuscado == a = True
          | otherwise = containsU elemBuscado b || containsU elemBuscado c


contains :: (Ord a) => a -> Arboles a -> Bool
contains buscado Void = False
contains buscado (Nod a b c)
 | buscado == a = True
 | buscado < a = contains buscado b
 | buscado > a = contains buscado c 


inorden :: Arboles a -> [a]
inorden Void = []
inorden (Hoj x)     = [x]
inorden (Nod x a b) = inorden a ++ (x : inorden b)


preorden :: Arboles a -> [a]
preorden Void = []
preorden (Hoj x)     = [x]
preorden (Nod x a b) = x : (preorden a ++ preorden b)


postorden :: Arboles a -> [a]
postorden  Void  = []
postorden (Hoj x)     = [x]
postorden (Nod x a b) = postorden a ++ postorden b ++ [x]


add :: (Ord a) => a -> Arboles a -> Arboles a
add nuevo Void = Nod nuevo Void Void
add nuevo (Nod a b c)
 | nuevo <= a = Nod a (add nuevo b) c
 | nuevo > a = Nod a b (add nuevo c)


fromList :: (Ord a) => [a] -> Arboles a
fromList [] = Void
fromList (raiz:sub) = Nod raiz (fromList (filter (<= raiz) sub)) (fromList (filter (> raiz) sub))





-- Árbol prueba 2
arb :: Arboles Int 
arb = Nod 1 Void (Nod 2 Void (Nod 3 Void (Nod 4 Void (Nod 5 Void Void))))

  

-- Árbol de prueba 1
test :: Arboles Int
test = (Nod 1
         (Nod 2
           (Nod 9
             (Void)
             (Nod 10 (Void) (Void))
           )
           (Nod 4
             (Nod 5 (Void) (Void))
             (Nod 6 (Void) (Void))
           )
         )
         (Nod 3
           (Nod 7 (Void) (Void))
           (Nod 8 (Void) (Void))
         )
       )