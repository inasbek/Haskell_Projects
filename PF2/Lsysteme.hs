module Main where

import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)

type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

type EtatDessin = (EtatTortue, Path)


-----------------------------------FONCTIONS------------------------------------

--Exo1
-- Recursivite :
motSuivant :: Regles -> Mot -> Mot
motSuivant _ [] = []
motSuivant r (x:xs) = (r x) ++ (motSuivant r xs)


-- Comprehension :
motSuivant' :: Regles -> Mot -> Mot
motSuivant' _ [] = []
motSuivant' regl mot = concat [regl lettre | lettre <- mot]


-- Prelude :
motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' _ [] = []
motSuivant'' r m = concatMap r m

regle :: Axiome
regle = "F"

-- VonKoch
--vonKoch :: Regles
--vonKoch _   = ""
--vonKoch '+' = "+"
--vonKoch '-' = "-"
--vonKoch 'F' = "F−F++F−F"

--Exo3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme axiome regl = iterate (motSuivant regl) axiome

-- take 3 (lsysteme "F" r)
-- ["F","F-F++F-F","F-F++F-F-F-F++F-F++F-F++F-F-F-F++F-F"]


--Exo4

etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x

longueurPas :: Config -> Float
longueurPas (_,x,_,_,_) = x

facteurEchelle :: Config -> Float
facteurEchelle (_,_,x,_,_) = x

angle :: Config -> Float
angle (_,_,_,x,_) = x

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,x) = x

--Exo5
--Avance
avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),a) = ((x',y'),a)
                    where x' = x + (longueurPas c)* cos(a)
                          y' = y + (longueurPas c)* sin(a)
--Exo 6
--tourne A Gauche
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c (point,a) = (point , a')
                          where a'= a + (angle c)

--tourne A Droite
tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite  c (point,a) = (point , a')
                          where a'= a - (angle c)
--Exo 7
--filtre Symboles Tortue
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue c m = [s | s <- m, s `elem` symbolesTortue c]

--Exo 8
--Interprétation d'un Symbole
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole cfg (etat, path) s = (etat', path ++ [fst etat'])
    where etat' | s == 'F'  = avance cfg etat
                | s == '+'  = tourneAGauche cfg etat
                | s == '-'  = tourneADroite cfg etat
                | otherwise = error "wrong symbol"

--Exo 10
-- Interprétation d'un mot
interpreteMot :: Config -> Mot -> Picture
interpreteMot c m = line (snd (foldl (interpreteSymbole c) iE mF))
                    where iP = fst (etatInitial c)
                          iE = (etatInitial c, [iP])
                          mF = filtreSymbolesTortue c m

--Exo 11
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime ls c t = interpreteMot conf (ls !! enieme)
                  where enieme = round t `mod` 8
                        conf = case c of
                        {(e, p, fE, a, s) -> (e, p * (fE ^ enieme), fE, a, s)}

---------------------------GENERATION DESSINS----------------------------------
vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
          where regles 'F' = "F-F++F-F"
                regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
          where regles 'F' = "F-F++F-F"
                regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
          where regles 'X' = "+YF-XFX-FY+"
                regles 'Y' = "-XF+YFY+FX-"
                regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
         where regles 'X' = "X+YF+"
               regles 'Y' = "-FX-Y"
               regles  s  = [s]

----------------------------AFFICHAGE DESSINS-----------------------------------
vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

dessin ::Picture
dessin = vonKoch2Anime 2--interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main :: IO ()
main = display (InWindow "Lsysteme" (1000, 1000) (0, 0)) white dessin
