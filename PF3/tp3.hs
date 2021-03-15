--import Test.QuickCheck
import Data.List
import Control.Concurrent (threadDelay)


(+++) :: [a]->[a]->[a]
(+++) xs ys = foldr (:) ys xs

concat' :: [[a]]->[a]
concat' xss  = foldr (++) [] xss


data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val) | Feuille
                      deriving Show
--data coul = Rouge | Noir
--            deriving Show


mapArbre :: (coul -> coul) -> (val -> val) -> Arbre coul val  -> Arbre coul val
mapArbre  coulfct valfct (Noeud coul val arbreR arbreL) = Noeud (coulfct coul) (valfct val) (mapArbre coulfct valfct arbreR) (mapArbre coulfct valfct arbreL)


hauteur :: Arbre coul val -> Int
hauteur Feuille = 0
hauteur (Noeud _ _ g d) = 1 + (hauteur g) `max` (hauteur d)

taille :: Arbre coul val -> Int
taille Feuille = 0
taille (Noeud _ _ g d) = 1 + taille g + taille d




dimension :: Arbre coul val -> (Int -> Int -> Int)->  Int
dimension Feuille _ = 0
dimension (Noeud _ _ g d) op  =  1 +  ((dimension g op) `op`  (dimension d op))


peigneGauche :: [(coul,val)] -> Arbre coul val
peigneGauche [] = Feuille
peigneGauche (x:xs) = Noeud (fst(x)) (snd(x)) (peigneGauche xs) (Feuille)


prop_hauteurPeigne :: [(coul,val)] -> Bool
prop_hauteurPeigne xs = (length xs == hauteur (peigneGauche xs))

--Exo7

prop_taillePeigne :: [(coul,val)] -> Bool
prop_taillePeigne xs = (length xs == taille (peigneGauche xs))


--Exo8

estComplet :: Arbre coul val -> Bool
estComplet Feuille = True
estComplet (Noeud _ _ g d) | (taille(g) == taille(d) && hauteur(g) == hauteur(d)) = estComplet(g) == estComplet(d)
                         | otherwise = False

--Exo9
prop_estCompletPeigne :: [(coul, val)] -> Bool
prop_estCompletPeigne xs = estComplet(peigneGauche xs)

--Exo10

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

--Exo11
complet :: Int -> [(coul, val)] -> Arbre coul val
complet 0 [] = Feuille
complet 1 [(a,b)] = (Noeud a b Feuille Feuille)
complet 2 (x:y:z:xs) = Noeud (fst(y)) (snd(y)) (complet 1 (x:xs)) (complet 1 (z:xs))
--complet n (x:y:z:xs) = Noeud (fst(head(middle(x:y:z:xs)))) (snd(head(middle(x:y:z:xs)))) (complet (n-1) (x:xs)) (complet (n-1) (z:xs))
complet n xs = Noeud (color) (valeur) (arbreG) (arbreD)
                where
                  color  = fst(xs !! (quot ((length xs) - 1) 2))
                  valeur = snd(xs !! (quot ((length xs) - 1) 2))
                  arbreG = complet (n-1) (take( quot ((length xs) - 1) 2) xs)
                  arbreD = complet (n-1) (drop( quot ((length xs) + 1) 2) xs)

--  'EXEMPLE complet 3 [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('j',7)]

            -- dont frget : ite !! est un opérateur qui renvoie le n-ième élément d'une liste).

--EXO12
repeter :: a -> [a]
repeter = iterate $ id
--
-- (iterate (1*) x)

--Exo13
allAlphabets :: [Char]
allAlphabets = iterate (\x -> (toEnum(fromEnum (x) +1))) 'a'

alphabetsInTAB :: [((),Char)]
alphabetsInTAB = map (\x ->((),x)) allAlphabets


--EXO14
aplatit :: Arbre coul val -> [(coul, val)]
aplatit Feuille = []
aplatit (Noeud c v g d) = (aplatit g) ++ [(c,v)] ++ (aplatit d)

--exemple
-- aplatit (Noeud 'd' 4 (Noeud 'b' 2 (Noeud 'a' 1 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille)) (Noeud 'f' 6 (Noeud 'e' 5 Feuille Feuille) (Noeud 'j' 7 Feuille Feuille)))
--[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('j',7)]


--EXO15
element :: Eq val => val -> Arbre coul val -> Bool
element _ Feuille = False
element val (Noeud c v g d) | val == v = True
                            |otherwise = (element val g) || (element val d)

-- *Main> element 4 (Noeud 'd' 4 (Noeud 'b' 2 (Noeud 'a' 1 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille)) (Noeud 'f' 6 (Noeud 'e' 5 Feuille Feuille) (Noeud 'j' 7 Feuille Feuille)))
--True
-- Main> element 10 (Noeud 'd' 4 (Noeud 'b' 2 (Noeud 'a' 1 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille)) (Noeud 'f' 6 (Noeud 'e' 5 Feuille Feuille) (Noeud 'j' 7 Feuille Feuille)))
--False

------------Affichage des arbres-------------

--Exo16

noeud :: (coul -> String) -> (val -> String) -> (coul,val) -> String
noeud vtoS ctoS (v,c) = vtoS v ++ ctoS c

-- Main> noeud show show ('r',1)
--"'r'1"

--Exo17

arcs :: Arbre coul val -> [(val,val)]
arcs (Noeud _ _ Feuille Feuille) = []
arcs Feuille =[]
arcs (Noeud c v (Noeud a b g1 g2) (Noeud x y d1 d2)) = [(v,b)] ++ [(v,y)] ++ (arcs g1) ++ (arcs g2) ++ (arcs d1) ++ (arcs d2)

--Example
-- arcs (Noeud 'a' 1 (Noeud 'b' 2 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille))
--[(1,2),(1,3)]

--Exo18
arc :: (a -> String) -> (a,a) -> String
arc toString (a,b) = (toString a ) ++ " -> " ++ (toString b )

--arc show ('a','b')
--"'a' -> 'b'"

--Exo19
dotise :: String -> (coul -> String) -> (val -> String) -> Arbre coul val -> String
dotise treeName fcoul fVal (Noeud a b g d) = "\n \n \n/* Entête */ \n "
                                          ++ "digraph " ++ show treeName ++ "{ \n "
                                          ++ "node [fontname = \"DejaVu-Sans\", shape = circle] \n\n"
                                          ++ "/* Liste des nœuds */\n"
                                          ++ unlines (map (noeud fcoul fVal) (aplatit(Noeud a b g d)))
                                          ++ "\n \n/* Liste des arcs */ \n"
                                          ++ unlines (map (arc fVal) (arcs (Noeud a b g d)))
                                          ++ "} \n"


{-putStr(dotise "complet3" show show (Noeud 'a' 1 (Noeud 'b' 2 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille)))

 
--Example
/* Entête */ 
 digraph "complet3"{ 
 node [fontname = "DejaVu-Sans", shape = circle] 

/* Liste des nœuds */
'b'2
'a'1
'c'3

 
/* Liste des arcs */ 
1 -> 2
1 -> 3
} 
-}

--------- Enfin de la couleur… !
--Exo20

elementR :: Ord val => val -> Arbre coul val -> Bool
elementR _ Feuille = False
elementR x (Noeud a b g d) | x == b = True
                           | x < b = elementR x g
                           | x > b = elementR x d 
                          -- | otherwise = True
                        
{-- Exemple
elementR 4 (Noeud 'a' 1 (Noeud 'b' 2 Feuille Feuille) (Noeud 'c' 3 Feuille (Noeud 'x' 4 Feuille Feuille)))
True

elementR 4 (Noeud 'a' 1 (Noeud 'b' 2 Feuille Feuille) (Noeud 'c' 3 (Noeud 'x' 4 Feuille Feuille) Feuille))
False  == le noeud 4 est dans l'arbre droit de 3 , donc ça ne fonctionne pas

CAR LE PRINCIPE DE L'ARBRE BINAIRE DE RCHERCHE EST TOUTES LES VALEURS DROITES SONT PLUS PETITES QUE LEURS NOEUDS , ET TOUS LES NOEUDS GAUCHES SONT PLUS GRANDS QUE LEUR NOEUDS
-}

-- EXO21
data Couleur = R | N 
              deriving (Eq,Show)
type ArbreRN a = Arbre Couleur a



colorToString :: Show c => c -> String
colorToString color = "[color=" ++ (show color) ++ ", fontcolor=" ++ (show color) ++ "]"

valueToString :: (Show a) => a -> String
valueToString value = removeQuote(show value)

removeQuote :: String -> String
removeQuote word = filter (not . (`elem` "\'")) word

--EXO22

equilibre :: Couleur -> a -> ArbreRN a -> ArbreRN a -> ArbreRN a
equilibre N z (Noeud R y (Noeud R x a b) c ) d = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre N z (Noeud R x a (Noeud R y b c)) d = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre N x a (Noeud R z (Noeud R y b c) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre N x a (Noeud R y b (Noeud R z c d)) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre c x g d = Noeud c x g d

{-
*Main> equilibre N 3 (Noeud R 2 Feuille Feuille) (Noeud R 4 Feuille (Noeud R 5 Feuille Feuille))
Noeud R 4 (Noeud N 3 (Noeud R 2 Feuille Feuille) Feuille) (Noeud N 5 Feuille Feuille)

-}

--EXO23

noeudToBlack :: ArbreRN a -> ArbreRN a
noeudToBlack (Noeud _ y g d) = Noeud N y g d
{-
insert ::   Ord a =>  a -> ArbreRN a -> ArbreRN a
insert v Feuille = Noeud R v Feuille Feuille
insert v (Noeud R y g d) | elementR v (Noeud R y g d) == True = Noeud R y g d
                         | v < y = equilibre R y noeudToBlack( (insert v g)) d
                         | v > y = equilibre R y g noeudToBlack( (insert v d))

-}

inserti :: (Ord a) => a -> ArbreRN a -> ArbreRN a
inserti x s = black $ ins s
  where ins Feuille  = Noeud R x Feuille Feuille
        ins (Noeud color y a b)
          | x < y  = equilibre color y (ins a) b
          | x == y = Noeud color y a b
          | otherwise  = equilibre color y a (ins b)
        black (Noeud _ y a b) = Noeud N y a b
{-
*Main> insert 6 (Noeud R 3 (Noeud R 2 Feuille Feuille) (Noeud R 4 Feuille (Noeud R 5 Feuille Feuille)))
Noeud N 3 (Noeud R 2 Feuille Feuille) (Noeud R 4 Feuille (Noeud R 5 Feuille (Noeud R 6 Feuille Feuille)))
-}

--EXO26
arbresDot :: (Show a, Ord a) => [a] -> [String]
arbresDot values = map (dotise "arbre" colorToString valueToString) treeList
                   where treeList = unfoldr (\(a,b) -> if (null b) 
                                                       then Nothing 
                                                       else Just (inserti (head b) a,(inserti (head b) a, (tail b)))) (Feuille, values) 





main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"


-- dot -Tps2 arbre.dot -o arbre.hs | ps2pdf arbre.hs