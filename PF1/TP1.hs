module TP1 where

-- Exercice 3
-- Fonction sommeDeXaY, fait une somme des entier ce trouvant entre x et y inclus

sommeDeXaY :: (Num a, Ord a) => a->a->a
sommeDeXaY x y = if x>y
                  then sommeDeXaY y x
                  else if x<y
                  then x + sommeDeXaY (x+1) y
                  else x;

-- Exercice 4
-- Fonction somme, fait une somme des entier de la liste passer en param
somme :: Num a => [a] -> a
somme [] = 0
somme (x:xs) = x + somme xs ;

-- Exercice 5
	--Last
last :: [t]->t
last t = head(reverse t);
	--Init
init :: [t]->[t]
init t = reverse (drop 1 (reverse t));

-- Exercice 6
	-- !!
recupValue :: (Num a,Ord a) => a->[t]->t
recupValue x (t:q) = if x>0
                      then (recupValue (x-1) q)
                      else t;

	-- ++
concatList :: [t]->[t]->[t]
concatList [t] list = t : list;
concatList (t1:q1) list = t1 : (concatList q1 list);

	-- concat
concatLists :: [[t]]->[t]
concatLists [] = []
concatLists (t:q) = concatList t (concatLists q)

	-- map
myMap :: (a->b)->[a]->[b]
myMap f [] = []
myMap f (t:q) = f t : (myMap f q)

-- Exercice 7
-- On affecte à x le début de la fonction (!!) avec une liste
-- Par conséquent, x attends maintenant un second argument qui est un entier
-- pour exécuter la fonction (!!) et retourner l'élément du tableau souhaité

-- Exercice 8
longueurListe :: [t]->Int
longueurListe l = somme (myMap (\x->1) l)

-- Exercice 9
uneFonction :: (Num a) => a->a
uneFonction x = x+1

ex9 :: (Num a) => (a->a)->a->Int->[a]
ex9 f x n = if n<=0 then [] else x : (ex9 f (f x) (n-1))

-- Exercice 10
consecutif :: Int->[Int]
consecutif n = ex9 succ 0 (n+1)
