module Algorithms where

import qualified Data.Set as S
import qualified Data.List as L
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

-- returneaza nodurile vecine nodului curent
getAdj node graph = foldr (\(x, y) -> (y :)) [] (getEdgesFromNode node graph)

-- operatiile uzuale pentru stiva
emptyStack = []
stackPush newElem stack = stack ++ [newElem]
 
-- operatiile uzuale pentru coada
emptyQueue = []
queueEnqueue :: a -> [a] -> [a]
queueEnqueue newElem queue = newElem : queue

-- peek si pop le-am facut la fel pentru ambele pentru a facilita functia search
peek structure = last structure
pop [] = []
pop structure = init structure

-- returneaza coada pentru bfs de la pasul curent in functie de vecini
makeBFS = (\queue adj -> foldr (\x -> queueEnqueue x) queue (reverse adj))
-- returneaza stiva pentru dfs de la pasul curent in functie de vecini
makeDFS = (\stack adj -> foldr (\x -> stackPush x) stack adj)

-- filtrez sa nu bag in structura (adica stiva sau coada) noduri deja vizitate
filterHelperStructure helperStructure visited = filter (\x -> if (elem x visited) == True
                                                                then False
                                                                else True)
                                                helperStructure

-- functie helper pentru a facilita search-ul
searchHelper f graph visited helperStructure = if ((null helperStructure) == False)
    -- verific daca nodul curent a fost vizitat
    then if ((elem node visited) == False)
        -- apelez recursiv si adaug nodul curent la vizitate, actualizez structura (si scot din ea elem deja vizitate)
        then searchHelper (f) graph (visited ++ [node]) (filterHelperStructure (f helperStructure (getAdj node graph)) visited)
        -- daca a fost vizitat, apelez functia recursiv, facand pop la nodul curent
        else searchHelper (f) graph visited (pop helperStructure)
    -- daca structura (stiva sau coada) este goala, returnez lista de noduri vizitate
    else visited
    where
        -- nodul curent o sa fie primul element din structura (stiva sau coada)
        node = peek helperStructure

-- folosesc functia de helper pentru a face search-ul
search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = searchHelper (f) graph []  [node] 

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}

-- apelez search dand ca parametru functia makeBFS, care prelucreaza coada
bfs :: Ord a => a -> Graph a -> [a]
bfs node graph = search makeBFS node graph

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}

-- apelez search dand ca parametru functia makeDFS, care prelucreaza stiva
dfs :: Ord a => a -> Graph a -> [a]
dfs node graph = search makeDFS node graph

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}

countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = case tuple of
    -- daca tuplul e o lista goala, returnez Nothing
    ([], [])  -> Nothing
    -- altfel daca nodul to apartine listelor bfs si dfs, returnez lungimea
    -- listelor din cadrul tuplului
    otherwise -> if ((elem to bfsResult) == True && (elem to dfsResult) == True)
                    then Just (length (fst tuple), length (snd tuple))
                    else Nothing
    where
        -- rezultatul bfs pe nodul from
        bfsResult = (bfs from graph)
        -- rezultatul dfs pe nodul from
        dfsResult = (dfs from graph)
        -- folosind span, fac un tuplu din rezultatul din lista din stanga pentru lista bfs
        -- si rezultatul din lista din stanga pentru lista dfs
        tuple = ((fst (span (/= to) (tail bfsResult))), (fst (span (/= to) (tail dfsResult))))
