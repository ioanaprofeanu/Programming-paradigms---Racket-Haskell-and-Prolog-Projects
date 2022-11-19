module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

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
-- daca cele doua noduri sunt conectate (adica aici se creeaza arcele)
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

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
-- sterge un nod din graf
removeNode node graph = recursiveRemoveNode graph
                        -- functie recursiva locala pe care facem pattern matching in functie de
                        -- tipul grafului
                    where recursiveRemoveNode Empty = Empty
                          -- atunci cand se ajunge la nod, verificam daca e egal cu nodul dat;
                          -- daca da, returnam Empty (il stergem); altfel, returnam nodul curent (adica nu il stergem)
                          recursiveRemoveNode (Node node2) = if (node == node2)
                                                                then Empty
                                                                else (Node node2)
                          -- pentru Overlay si Connect, se apeleaza functia recursiv pentru cele doua grafuri,
                          -- "unindu-se" intre ele cu Overlay/Connect
                          recursiveRemoveNode (Overlay graph1 graph2) = (Overlay (recursiveRemoveNode graph1) (recursiveRemoveNode graph2))
                          recursiveRemoveNode (Connect graph1 graph2) = (Connect (recursiveRemoveNode graph1) (recursiveRemoveNode graph2))
{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
-- inlocuieste un nod in mai multe noduri
splitNode old news graph = recursivesplitNode news graph
                        -- functie recursiva locala pe care facem pattern matching in functie de
                        -- tipul grafului
                    where recursivesplitNode news Empty = Empty
                          -- atunci cand se ajunge la nod, verificam daca e egal cu nodul vechi;
                          recursivesplitNode news (Node node) = if (node == old)
                                                                    -- daca lungimea listei de noduri noi e mai mare ca 1
                                                                    then if (length news) > 1
                                                                        -- facem Overlay (le conectam fara sa facem arce) intre primul element
                                                                        -- din lista news si apelul recursiv al functiei (la care news o sa fie
                                                                        -- restul listei)
                                                                        then Overlay (Node (head news)) (recursivesplitNode (tail news) (Node old))
                                                                        -- daca lungimea e 1
                                                                        else if (length news) == 1
                                                                            -- returnam nodul din lisa news
                                                                            then Node (head news)
                                                                            -- daca lista e goala, returnam Empty
                                                                            else Empty
                                                                    -- altfel, daca nodul nu e egal cu old, il returnam
                                                                    else (Node node)
                          -- pentru Overlay si Connect, se apeleaza functia recursiv pentru cele doua grafuri,
                          -- "unindu-se" intre ele cu Overlay/Connect
                          recursivesplitNode news (Overlay graph1 graph2) = (Overlay (recursivesplitNode news graph1) (recursivesplitNode news graph2))
                          recursivesplitNode news (Connect graph1 graph2) = (Connect (recursivesplitNode news graph1) (recursivesplitNode news graph2))


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
-- inlocuieste nodurile cu o proprietate cu un nod dat
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = recursiveMergeNodes graph
                                -- functie recursiva locala pe care facem pattern matching in functie de
                                -- tipul grafului
                            where recursiveMergeNodes Empty = Empty
                                  -- daca proprietatea este True pe nodul curent, returnam nodul nou dat;
                                  -- (adica il inlocuim cu nodul nou), altfel returnam tot nodul curent
                                  recursiveMergeNodes (Node node2) = if (prop node2 == True)
                                                                        then (Node node)
                                                                        else (Node node2)
                                  -- pentru Overlay si Connect, se apeleaza functia recursiv pentru cele doua grafuri,
                                  -- "unindu-se" intre ele cu Overlay/Connect
                                  recursiveMergeNodes (Overlay graph1 graph2) = (Overlay (recursiveMergeNodes graph1) (recursiveMergeNodes graph2))
                                  recursiveMergeNodes (Connect graph1 graph2) = (Connect (recursiveMergeNodes graph1) (recursiveMergeNodes graph2))
