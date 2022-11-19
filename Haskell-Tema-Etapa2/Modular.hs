module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
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
