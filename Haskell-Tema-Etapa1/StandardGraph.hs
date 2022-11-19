{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S
import qualified Data.List as L

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}

-- vom face un tuplu format din:
-- primul element (lista nodurilor) este set-ul format din prima lista data ca parametru
-- al doilea element (lista arcelor) este set-ul format din a doua lista data ca parametru
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
-- nodurile o sa fie primul element din tuplul graf
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}

-- muchiile o sa fie al doilea element din tuplul graf
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
-- obtine lista arcelor care pornesc din nodul dat
-- filter-ul ia doar tuplurile care au ca prim element nodul dat
getEdgesFromNode node graph = filter (\(x, y) -> if (x == node)
                                                    then True
                                                    else False)
                            (S.toList (edges graph))

-- din lista de arce care pleaca din nodul dat, formam lista care contine doar
-- al doilea element din tuplul curent
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList (foldr (\(x, y) -> (y :)) [] (getEdgesFromNode node graph))

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}

-- obtine lista arcelor care pornesc inspre nodul dat
-- filter-ul ia doar tuplurile care au ca al doilea element nodul dat
getEdgesToNode node graph = filter (\(x, y) -> if (y == node)
                                                    then True
                                                    else False)
                            (S.toList (edges graph))

-- din lista de arce care pleaca inspre nodul dat, formam lista care contine doar
-- al primul element din tuplul curent
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList (foldr (\(x, y) -> (x :)) [] (getEdgesToNode node graph))

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}

-- sterge nodul din lista de noduri
deleteFromNodes node graph = L.delete node (S.toList (nodes graph))
-- cu filter, din lista de noduri pastreaza doar tuplurile care nu contin
-- nodul dat (pe orice pozitie)
deleteEdges node graph = filter (\(x, y) -> if (x == node || y == node)
                                                then False
                                                else True)
                        (S.toList (edges graph))

-- formeaza noul graf cu noua lista de noduri si noua lista de arce
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = fromComponents (deleteFromNodes node graph) (deleteEdges node graph)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}

-- sterge nodul vechi folosind deleteFromNodes si adauga noile noduri la lista de noduri
splitChangeNodes old news graph = foldr (:) (deleteFromNodes old graph) news

-- intr-o lista noua, adaug noile arce care au ca prim element unul din nodurile din news,
-- si al doilea element un nod dat
splitFromChangeEdge node news = foldr (\x -> ((x, node) :)) [] news
-- folosind lista arcelor care pleaca din nodul vechi, pentru fiecare nod inspre care se duce arcul (adica nodul y),
-- apelam splitFromChangeEdge pentru a forma lista cu arce de forma (y, nod din news)
splitFromChangeEdges old news graph = foldr (\(x, y) -> ((splitFromChangeEdge y news) ++)) [] (getEdgesFromNode old graph)

-- intr-o lista noua, adaug noile arce care au ca al doilea element unul din nodurile din news,
-- si al primul element un nod dat
splitToChangeEdge node news = foldr (\x -> ((node, x) :)) [] news
-- folosind lista arcelor care se duc spre nodul vechi, pentru fiecare nod din care se duce arcul (adica nodul x),
-- apelam splitFromChangeEdge pentru a forma lista cu arce de forma (nod din news, x)
splitToChangeEdges old news graph = foldr (\(x, y) -> ((splitToChangeEdge x news) ++)) [] (getEdgesToNode old graph)

-- formez un nou graf care are noua lista de noduri si arcele care contin arcele de dinainte fara arcele ce contin
-- nodul old + noile arce care pornesc din nodurile din lista news + cele care ajung 
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = fromComponents (splitChangeNodes old news graph) ((deleteEdges old graph)
                            ++ (splitFromChangeEdges old news graph)
                            ++ (splitToChangeEdges old news graph))

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 ([1,2,3,4],[(1,2),(1,3),(1,4),(2,3),(4,1)]) graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}

-- iau lista de noduri si pastrez, cu filter, doar nodurile care NU au proprietatea data; la lista obtinuta, adaug nodul nou
mergeDeleteAndAdd prop node graph = node : (filter (\x -> if prop x == True
                                                            then False
                                                            else True)
                                    (S.toList (nodes graph)))
-- folosind lista initiala de noduri, returnez nodurile pe care le-am sters
mergeGetDeleted prop node graph = filter prop (S.toList (nodes graph))

-- iau lista de arce si cu filter pastrez doar arcele care NU au unul din nodurile pe care le-am sters
mergeDeleteEdges prop node graph = filter (\(x, y) -> if ((L.elem x deleted) == True || (L.elem y deleted) == True)
                                                        then False
                                                        else True)
                                    (S.toList (edges graph))
                                where deleted = (mergeGetDeleted prop node graph)

-- in oldSameEdge tin arcele care contin ambele noduri din nodurile pe care le-am sters (le
-- obtin luand lista veche de arce si verificand daca ambele apartin listei old de noduri sterse)
-- folosind lista oldSameEdge, verific daca lungimea ei e 0; daca da, returnez [], altfel,
-- returnez lista care are un arc cu ambele noduri egale cu new
mergedInSameEdge olds new graph = if length oldSameEdge == 0
                                    then []
                                    else [(new, new)]
                                where oldSameEdge = filter (\(x, y) -> if (elem x olds) && (elem y olds)
                                                                        then True
                                                                        else False)
                                                    (S.toList (edges graph))
-- iau lista de noduri pe care le-am sters, formez noduri noi folosind functia de la splitFrom                                                              
mergeFromChangeEdges olds new graph = (foldr (\x -> ((splitFromChangeEdges x (new : []) graph) ++)) [] olds)
-- iau lista de noduri pe care le-am sters, formez noduri noi folosind functia de la splitTo
mergeToChangeEdges olds new graph = foldr (\x -> ((splitToChangeEdges x (new : []) graph) ++)) [] olds

-- merge all the previously calculated edges
mergeEdges prop node graph = ((mergeDeleteEdges prop node graph)
                                ++ (mergeFromChangeEdges deletedNodesList node graph)
                                ++ (mergeToChangeEdges deletedNodesList node graph)
                                ++ (mergedInSameEdge deletedNodesList node graph))
                            where deletedNodesList = (mergeGetDeleted prop node graph)

-- if any edges have the deleted node, remove them
removeOldEdges old prop node graph = filter (\(x, y) -> if (elem x old || elem y old)
                                                            then False
                                                            else True)
                                    (mergeEdges prop node graph)

-- if there are any deleted nodes, then remake the graph with the new components
-- otherwise, return the initial graph
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = if length deletedNodesList == 0
                                then graph
                                else fromComponents (mergeDeleteAndAdd prop node graph)
                                    (removeOldEdges deletedNodesList prop node graph)
                            where deletedNodesList = (mergeGetDeleted prop node graph)
