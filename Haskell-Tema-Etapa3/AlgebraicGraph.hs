module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
-- returneaza nodurile grafului
nodes :: Ord a => AlgebraicGraph a -> S.Set a
-- returneaza set-ul din lista goala
nodes Empty = S.fromList []
-- returneaza set-ul din lista cu un nod
nodes (Node node) = S.fromList [node]
-- uneste listele obtinute din apelurile recursive pe cele doua subgrafuri
nodes (Overlay graph1 graph2) = S.union (nodes graph1) (nodes graph2)
nodes (Connect graph1 graph2) = S.union (nodes graph1) (nodes graph2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
-- returneaza arcele grafului
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
-- returneaza set-ul din lista goala
edges Empty = S.fromList []
-- returneaza set-ul din lista goala
edges (Node node) = S.fromList []
-- uneste listele obtinute din apelurile recursive pe cele doua subgrafuri
edges (Overlay graph1 graph2) = S.union (edges graph1) (edges graph2)
-- uneste produsul cartezia al nodurilor din primul subgraf cu cele din al doilea (practic
-- toate nodurile din cele doua subgrafuri au arce intre ele) si lista obtinuta din apelul
-- recursiv pe cele doua subgrafuri
edges (Connect graph1 graph2) = S.union (S.cartesianProduct nodesGraph1 nodesGraph2) (S.union (edges graph1) (edges graph2))
                            -- nodurile celor doua grafuri
                            where nodesGraph1 = nodes graph1
                                  nodesGraph2 = nodes graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
-- returneaza lista cu nodurile destinatie ce au ca sursa nodul node
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
-- returneaza set-ul din lista goala
outNeighbors node Empty = S.fromList []
-- returneaza set-ul din lista goala
outNeighbors node (Node node2) = S.fromList []
-- uneste listele obtinute din apelurile recursive pe cele doua subgrafuri
outNeighbors node (Overlay graph1 graph2) = S.union (outNeighbors node graph1) (outNeighbors node graph2)
-- daca cle doua noduri sunt conectate (adica aici se creeaza arcele)
outNeighbors node (Connect graph1 graph2) = if (elem node nodesGraph1)
                                                -- daca node se afla in primul subgraf, unesc toate nodurile din
                                                -- al doilea graf cu listele obtinute din apelul recursiv
                                                then S.union nodesGraph2 recursiveCall
                                                -- altfel continui apelul recursiv
                                                else recursiveCall
                                                 -- nodurile celor doua grafuri
                                            where nodesGraph1 = nodes graph1
                                                  nodesGraph2 = nodes graph2
                                                  -- uneste listele obtinute din apelul recursiv pe cele doua subgrafuri
                                                  recursiveCall = S.union (outNeighbors node graph1) (outNeighbors node graph2)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
-- returneaza set-ul din lista goala
inNeighbors node Empty = S.fromList []
-- returneaza set-ul din lista goala
inNeighbors node (Node node2) = S.fromList []
-- uneste listele obtinute din apelurile recursive pe cele doua subgrafuri
inNeighbors node (Overlay graph1 graph2) = S.union (inNeighbors node graph1) (inNeighbors node graph2)
-- daca cle doua noduri sunt conectate (adica aici se creeaza arcele)
inNeighbors node (Connect graph1 graph2) = if (elem node nodesGraph2)
                                                -- daca node se afla in al doilea subgraf, unesc toate nodurile din
                                                -- primul graf cu listele obtinute din apelul recursiv
                                                then S.union nodesGraph1 recursiveCall
                                                -- altfel continui apelul recursiv
                                                else recursiveCall
                                                -- nodurile celor doua grafuri
                                            where nodesGraph1 = nodes graph1
                                                  nodesGraph2 = nodes graph2
                                                  -- uneste listele obtinute din apelul recursiv pe cele doua subgrafuri
                                                  recursiveCall = S.union (inNeighbors node graph1) (inNeighbors node graph2)


{-
    *** TODO ***

    Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:
    - un literal întreg să fie interpretat ca un singur nod cu eticheta egală
      cu acel literal
    - operația de adunare să fie intepretată ca Overlay
    - operația de înmulțire să fie interpretată drept Connect.

    Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
    pentru neimplementarea lor, dar puteți să le ignorați.

    După instanțiere, veți putea evalua în consolă expresii ca:

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger no = Node(fromInteger no)
    (+) no1 no2 = Overlay no1 no2
    (*) no1 no2 = Connect no1 no2

{-
    *** TODO ***

    Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât
    reprezentarea sub formă de șir de caractere a unui graf să reflecte
    expresiile aritmetice definite mai sus. Puteți pune un nou rând de paranteze
    la fiecare subexpresie compusă.

    Exemple:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show (Node node) = show node
    show (Overlay graph1 graph2) = "(" ++ show graph1 ++ "+" ++ show graph2 ++ ")"
    show (Connect graph1 graph2) = "(" ++ show graph1 ++ "*" ++ show graph2 ++ ")"

{-
    *** TODO ***

    Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
    nu surprinde corect egalitatea a două grafuri, deoarece același graf
    conceptual poate avea două descrieri simbolice diferite.
    
    Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
    să comparați propriu-zis mulțimile de noduri și de arce.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
-- iau lista de noduri din g1 si g2 si verific daca sunt egale; la fel si cu muchiile
instance Ord a => Eq (AlgebraicGraph a) where
    (==) g1 g2 = nodes g1 == nodes g2 && edges g1 == edges g2
    (/=) g1 g2 = not ((==) g1 g2)
{-
    *** TODO ***

    Extinde un graf existent, atașând noi subgrafuri arbitrare în locul nodurilor
    individuale. Funcția primită ca prim parametru determină această
    corespondență între noduri și subgrafuri. Observați că tipul etichetelor
    noi (b) poate diferi de al etichetelor vechi (a).

    Exemplu:

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f graph = recursiveExtend graph
            where recursiveExtend Empty = Empty
                  recursiveExtend (Node node) = f node
                  recursiveExtend (Overlay graph1 graph2) = (Overlay (recursiveExtend graph1) (recursiveExtend graph2))
                  recursiveExtend (Connect graph1 graph2) = (Connect (recursiveExtend graph1) (recursiveExtend graph2))

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Implementați splitNode folosind extend!
-}
-- functie care ia fiecare nod din targets si face Oveerlay intre ele
makeSplit targets = case (length targets) of
                        0 -> Empty
                        1 -> Node (head targets)
                        otherwise ->  Overlay (Node (head targets)) (makeSplit (tail targets))

-- aplica extend, dandu-i functia lamda care verifica daca nodul curent
-- e egal cu node; daca da, aplica functia make split, altfel ramane nodul care era
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets = (\graph -> extend (\x -> if x == node
                                                    then (makeSplit targets)
                                                    else Node x) graph)

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel
    încât să puteți aplica o funcție pe toate etichetele unui graf.
    fmap reprezintă generalizarea lui map pentru orice fel de structură.

    Implementați fmap folosind extend!

    Exemplu:

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}
-- aplica extend, iar functia data este aplicarea lui f pe node
instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    -- trebuie sa transform functia f din (a->b) in (a->AlgebraicGraph b)
    fmap f graph = extend (\node -> Node (f node)) graph

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Implementați mergeNodes folosind fmap!
-}
-- folosesc fmap caruia ii dau functia care verifica daca se respecta proprietatea;
-- daca da, atunci returnez nodul nod, altfel ramane celalalt
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node = (\graph -> fmap (\x -> if (prop x) == True
                                                then node
                                                else x) graph)

{-
    *** TODO ***

    Filtrează un graf, păstrând doar nodurile care satisfac proprietatea dată.

    Implementați filterGraph folosind extend!
    
    Exemplu:

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
-- folosesc extend caruia ii dau functia care verifica daca se respecta proprietatea,
-- daca da, returnez nodul x, altfel returez nodul null (empty)
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\x -> if (prop x) == True
                                            then Node x
                                            else Empty) graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Implementați removeNode folosind filterGraph!
-}
-- folosesc filter cu functia care verifica daca nodul curent este egal cu node;
-- daca da, returneaza False, altfel True
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (\x -> if (x == node) == True
                                                then False
                                                else True) graph
