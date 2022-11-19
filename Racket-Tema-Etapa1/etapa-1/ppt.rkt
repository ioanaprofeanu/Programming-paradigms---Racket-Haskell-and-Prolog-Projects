#lang racket

(provide (all-defined-out))
(require (lib "trace.ss"))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.

; Ideea de rezolvare:
; dot-product [] [] = 0
; dot-product (x:xs) (y:ys) = x*y + dot-product(xs, ys)
(define (dot-product X Y)
  (if (null? X)
       0
       (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))

; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.

; folosesc functia ajutatoare recursiva care "mimeaza" un for printre liniile matricei
; in result o sa retin rezultatul pe parcurs, acesta fiind un append intre rezultatul anterior si produsul
; scalar al liniei curente din matrice si vectorul V; iterarea se termina atunci cand contorul ajunge la
; valoarea dimensiunii vectorului V

(define (multiply M V)
  (tail-multiply M V '() 0 (length V)))

(define (tail-multiply M V result counter max-counter)
  (if (equal? counter max-counter)
      result
      (tail-multiply (cdr M) V (append result (list (dot-product (car M) V))) (+ 1 counter) max-counter)))

; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

; gaseste nivelul pe care se afla n, comparand n cu suma curenta de puteri ale lui 3
(define (get-tree-level n level sum)
  (if (<= n sum)
      level
      (get-tree-level n (+ level 1) (+ sum (expt 3 (+ level 1))))))

; functie care face suma puterilor lui 3, pentru o putere data (3^0 + 3^1 +...+ 3^power); daca puterea e negativa, rezultatul e considerat 1
(define (sum-3-power power)
  (if (<= power 0)
      1
      (+ (expt 3 power) (sum-3-power (- power 1)))))

; pentru tripletul cu indicele n, returneaza din ce transformare a fost obtinuta, in functie de restul impartirii la 3
(define (get-current-transformation n)
  (cond
    ((= (modulo n 3) 2) 1)
    ((= (modulo n 3) 0) 2)
    ((= (modulo n 3) 1) 3)))

; obtine transformarile prin care se trece pentru a se ajunge la al n-lea triplet, astfel:
; la fiecare pas curent, n = (3^0 + 3^1 +...+ 3^(level-2) + 1) + {[n - ((3^0 + 3^1 +...+ 3^(level-1) + 1)] : 3} -> practic, (3^0 + 3^1 +...+ 3^(level-1) + 1)
; reprezinta indicele minim al tripletului de pe nivelul curent; scad din n acest minim si il impart la 3,
; obtinand astfel al catelea triplet este tripletul "parinte" din nivelul acestuia. Pentru a afla valoarea exacta
; a parintelui (adica tripletul pe care am facut transformarea pentru a se ajunge la tripletul curent), adun la pozitia parintelui (3^0 + 3^1 +...+ 3^(level-2) + 1),
; adica minimul de pe nivelul parintelui;
; continuand, la fiecare pas curent, scad 1 din level, iar la lista rezultat adaug la inceput tipul de transformare din care s-a obtinut (1, 2 sau 3),
; folosind restul impartirii la 3 recursivitatea se termina atunci cand nivelul curent este mai mic de 0, in acest caz returnand rezultatul
(define (get-transformations-helper n level result-list)
  (if (< level 1)
      result-list
      (get-transformations-helper (+ (+ (sum-3-power (- level 2)) 1) (exact-floor(/ (- n (+ (sum-3-power (- level 1)) 1)) 3))) (- level 1) (cons (get-current-transformation n) result-list))))

; aplica functia helper de obtinere a transformarilor
(define (get-transformations n)
  (get-transformations-helper n (get-tree-level n 0 1) '()))

; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.

; la fiecare pas, luam primul element din Ts si vedem pe ce caz de transformare ne aflam; apoi, apelam recursiv functia apply-matrix-transformations
; pentru restul listei (fara primul element) si pentru noul ppt, care va fi obtinut prin multiplicarea matricei de transformare aferenta cu
; tripletul pitagoreic curent; ne oprim din recursivitate atunci cand lista Ts este nula

; functie care determina tipul transformarii, in functie de care este primul element din Ts
(define (get-transformation-type Ts-first-element)
  (cond
    ((= Ts-first-element 1) T1)
    ((= Ts-first-element 2) T2)
    ((= Ts-first-element 3) T3)))

; functie care aplica transformarea; in ppt o sa se afle rezultatul final, ppt inmultindu-se cu transformarea curenta,
; folosind functia multiply, la fiecare apel recursiv
; din transformare -> in ppt
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (apply-matrix-transformations (cdr Ts) (multiply (get-transformation-type (car Ts)) ppt))))

; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
; din n -> in ppt

; apelam functia de aplicare a transformarilor, avand ca parametrii transformarile prin care se trece pentru a ajunge la al n-lea
; triplet (obtinut prin aplicarea functiei get-transformations pe parametrul n) si tripletul de baza '(3 4 5)
(define ppt '(3 4 5))
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) ppt))
