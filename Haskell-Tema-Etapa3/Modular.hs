module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import AlgebraicGraph

type Graph a = AlgebraicGraph a

-- Graful descris în diagrama din enunțul temei
diagram :: AlgebraicGraph Int
diagram = ((1*2) * (3+4)) * 5

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+ 10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = recursiveMapSingle 0 ((length xs) - 1) []
                -- functie recursiva care, la fiecare apel recursiv, construieste o lista si o adauga la rezultat
                where recursiveMapSingle leftIndex rightIndex result = if (length xs == leftIndex)
                                                            then result
                                                            -- crestem leftIndex, scadem rightindex si adaugam lista la rezultat
                                                            else recursiveMapSingle (leftIndex + 1) (rightIndex - 1) (result ++ [combineParts leftIndex rightIndex])
                      -- functie pentru returnarea al n lea element din lista
                      getN n = xs!!n
                      -- functie pentru returnarea primelor n elemente din lista
                      firstN n = take n xs
                      -- functie pentru returnarea ultimelor n elemente din lista
                      lastN n = drop (length xs - n) xs
                      -- funcie care combina cele trei liste: lista cu primele leftIndex elemente, lista care contine elementul
                      -- leftIndex asupra caruia aplicam f si urmatoarele elemente (adica ultimele rightIndex elemente)
                      combineParts leftIndex rightIndex = ((firstN leftIndex) ++ [(f (getN leftIndex))] ++ (lastN rightIndex))       

{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}
partitions :: [a] -> [[[a]]]
partitions [] = [[]]
-- intai luam primul element (x) si il adaugam in aceeasi lista cu restul partitiilor (p este apelul recursiv)
-- si la lista mare se adauga partitiile care au aplicarea lui mapSingle pe x
partitions (x:xs) = [[[x]] ++ p | p <- partitions xs] ++ (partitionsMapSingle x (partitions xs) [] 0)

-- partitionsMapSingle 1 [[[2], [3]], [[2, 3]]] [] 0 => [[[1,2],[3]],[[2],[1,3]],[[1,2,3]]]
-- il ia pe x si aplica mapSingle pe restul listei (il pune pe x la )
partitionsMapSingle x xs result index = if (index == length xs)
                                            -- daca indexul e egal cu lungimea lui xs
                                            then result
                                            -- adaug la result rezultatul mapSingle care adauga elementul x in fata
                                            -- fiecarui element din lista din xs aflata la indicele index
                                            -- xs e lista de liste si vrem sa il punem pe x in fata fiecarui
                                            -- element (adica fiecarei liste) din lista xs
                                            else partitionsMapSingle x xs (result ++ ((mapSingle ([x] ++) (xs!!index)))) (index + 1)


{-
    *** TODO ***

    Verifică dacă o mulțime este un modul, i.e. dacă toate nodurile din mulțime
    au aceeași mulțime de vecini out și aceeași mulțime de vecini in,
    în exteriorul mulțimii de plecare. Cu alte cuvinte, excludem din verificare
    vecinii din interiorul mulțimii de plecare.

    Hint: S.map poate reduce dimensiunea unei mulțimi dacă elemente diferite
    inițial sunt asociate cu același element final, întrucât nu pot exista
    duplicate.

    Exemple:

    > isModule (S.fromList [1,2,3,4]) diagram
    True

    > isModule (S.fromList [5]) diagram
    True
    
    > isModule (S.fromList [1,2]) diagram
    True

    > isModule (S.fromList [3,4]) diagram
    True

    > isModule (S.fromList [1,3]) diagram
    False
-}
-- functie care ia un o lista de vecini si pastreaza doar vecinii care
-- nu sunt in set
neighNotInModule set neigh = S.filter (\x -> if (elem x set) == True
                                                then False
                                                else True) neigh

-- pentru vecinii in si out, folosesc S.map pentru fiecare element din set,
-- cu functia care pt fiecare element din set, ia vecinii si pastreaza doar pe cei
-- care nu sunt in set;
-- daca rezultatele au dimensiunea 1, inseamna ca nu e niciun element din set care
-- sa aiba alti vecini, deci e modul
isModule :: Ord a
         => S.Set a
         -> Graph a
         -> Bool
isModule set graph = if ((S.size outNeigh) == 1 && (S.size inNeigh) == 1)
                        then True
                        else False
            where outNeigh =  S.map (\x -> neighNotInModule set (outNeighbors x graph)) set
                  inNeigh =  S.map (\x -> neighNotInModule set (inNeighbors x graph)) set

{-
    *** TODO ***

    Verifică dacă o partiție a mulțimii de noduri constituie o descompunere
    modulară. Partiția este reprezentată ca o mulțime de mulțimi.

    Hint: la fel ca la isModule.

    Exemple:

    > isModularPartition
        (S.fromList [S.fromList [1], S.fromList [2],
                     S.fromList [3], S.fromList [4], S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4,5]]) diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4], S.fromList [5]])
                         diagram
    True

    > isModularPartition
        (S.fromList [S.fromList [1,2], S.fromList [3,4],
                     S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,3], S.fromList [2,4,5]])
                         diagram
    False
-}
-- ia fiecare element din partitie si face S.map, in care verifica daca
-- partitia curenta e modulara; apoi daca rezultatul are size 1,
-- daca elementul din set e True, returneaza True; altfel False
isModularPartition :: Ord a
                   => Partition a
                   -> Graph a
                   -> Bool
isModularPartition partition graph = if (S.size isPartitionModule == 1) then
                                        if (head (S.toList isPartitionModule) == True)
                                            then True
                                            else False
                                        else False
                        where isPartitionModule = S.map (\x -> isModule x graph) partition

{-
    *** TODO ***

    Determină partiția maximală dintr-o listă de partiții. Partiția maximală
    conține cele mai acoperitoare submulțimi ale mulțimii de noduri. Cu alte
    cuvinte, partiția maximală conține cel mai mic număr de submulțimi mai mare
    strict decât 1, pentru a exlcude partiția care conține doar întreaga mulțime
    de noduri.

    Hint: minimumBy din Data.List. Funcția este folosită pentru a stabili
    un criteriu ad hoc de ordonare, conform valorii întoarse de o funcție f
    când este aplicată pe elementele listei, printr-o construcție de forma:

    minimumBy (compare `on` f) lista.

    Exemple:

    > maximalModularPartition <lista partițiilor> diagram
    fromList [fromList [1,2,3,4],fromList [5]]

    > maximalModularPartition <lista partițiilor> $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
-- folosesc minimumBy cu functia length pe lista filtrata cu toate partitiile care sunt
-- modulare si care nu au lungimea 1
maximalModularPartition :: Ord a
                        => [Partition a]
                        -> Graph a
                        -> Partition a
maximalModularPartition partitions graph = minimumBy (compare `on` length) (filter (\x -> if length x == 1 then False 
                                                                            else if (isModularPartition x graph) == True
                                                                                        then True
                                                                                        else False) partitions)
{-
    Obține descompunerea modulară a unui graf. O puteți utiliza pentru
    a experimenta manual cu maximalModularPartition.
    
    Exemple:

    > modularlyDecompose diagram                        
    fromList [fromList [1,2,3,4],fromList [5]]

    > modularlyDecompose $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
modularlyDecompose :: Ord a
                   => Graph a
                   -> Partition a
modularlyDecompose graph = maximalModularPartition partList graph
  where
    parts = partitions $ S.toList $ nodes graph
    partList = map (S.fromList . map S.fromList) parts