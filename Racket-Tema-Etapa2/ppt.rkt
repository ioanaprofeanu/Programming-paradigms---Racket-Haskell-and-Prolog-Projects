#lang racket

(provide (all-defined-out))
(require (lib "trace.ss"))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15

; initial, cu map * inmultim elementele celor doua liste intre ele; apoi, cu apply + facem suma lor
(define (dot-product X Y)
  (apply + (map * X Y)))

; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|

; am folosit foldl utilizand o functie lambda care face append intre acumulatorul '() si lista formata din rezultatul
; functiei dot-product pentru linia curenta din M si lista V
(define (multiply M V)
  (foldl (lambda (X Y) (append Y (list (dot-product X V)))) '() M))

; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

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
; la fiecare pas curent, n = (3^0 + 3^1 +...+ 3^(level-2) + 1) + {[n - ((3^0 + 3^1 +...+ 3^(level-1) + 1)] : 3} -> practic, (3^0 + 3^1 +...+ 3^(level-1) + 1) reprezinta indicele minim al
; tripletului de pe nivelul curent; scad din n acest minim si il impart la 3, obtinand astfel al catelea triplet este tripletul "parinte" din nivelul acestuia. Pentru a afla valoarea exacta
; a parintelui (adica tripletul pe care am facut transformarea pentru a se ajunge la tripletul curent), adun la pozitia parintelui (3^0 + 3^1 +...+ 3^(level-2) + 1), adica minimul de pe
; nivelul parintelui;
; continuand, la fiecare pas curent, scad 1 din level, iar la lista rezultat adaug la inceput tipul de transformare din care s-a obtinut (1, 2 sau 3), folosind restul impartirii la 3
; recursivitatea se termina atunci cand nivelul curent este mai mic de 0, in acest caz returnand rezultatul
(define (get-transformations-helper n level result-list)
  (if (< level 1)
      result-list
      (get-transformations-helper (+ (+ (sum-3-power (- level 2)) 1) (exact-floor(/ (- n (+ (sum-3-power (- level 1)) 1)) 3))) (- level 1) (cons (get-current-transformation n) result-list))))

; aplica functia helper de obtinere a transformarilor
(define (get-transformations n)
  (get-transformations-helper n (get-tree-level n 0 1) '()))

; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).

; am aplicat foldl, unde tuple este acumulatorul in care o sa avem rezultatul, avand functia lambda
; care aplica practic linia curenta (ce e o functie) din Fs pe rezultatul actual aflat in acumulator
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (X Y) (X Y)) tuple Fs))

; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.

; !!! Observatie: functia get-transformations este valida atat pentru triplete, cat si pentru cvartete

; functii pentru cvartete care va aplica transformarea specifica pe o lista initiala de tupluri (care va fi de fapt un cvartet)
(define (FQ1 tuple) (apply Q1 tuple))
(define (FQ2 tuple) (apply Q2 tuple))
(define (FQ3 tuple) (apply Q3 tuple))

; functii pentru triplete care va aplica transformarea specifica pe o lista initiala de tupluri (care va fi de fapt un triplet)
(define (FT1 tuple) (multiply T1 tuple))
(define (FT2 tuple) (multiply T2 tuple))
(define (FT3 tuple) (multiply T3 tuple))

; functii care returneaza functia ce efectueaza transformarile FT/ FQ (1 2 sau 3, in functie de parametrul primit)
(define (function-type-triplets Ts-first-element)
  (cond
    ((= Ts-first-element 1) FT1)
    ((= Ts-first-element 2) FT2)
    ((= Ts-first-element 3) FT3)))

(define (function-type-quadruple Ts-first-element)
  (cond
    ((= Ts-first-element 1) FQ1)
    ((= Ts-first-element 2) FQ2)
    ((= Ts-first-element 3) FQ3)))

; functie care transforma o lista in tipul celei returnate de get-transformations si o transforma intr-o lista de functii care
; efectiueaza transformarea (lista de proceduri), adica returneaza o noua lista care are la inceputul fiecarui element FQ sau FT
; ex: '(1 2 3) -> '(FT1 FT2 FT3); '(1 2 3 4) -> '(FQ1 FQ2 FQ3 FQ4)
(define (get-functions-list Ts acc function-type)
  (if (null? Ts)
      (reverse acc)
      (get-functions-list (cdr Ts) (cons (function-type (car Ts)) acc) function-type)))

; functie curry, unde prima functie lambda are ca parametrii n si tipul functiei dorite (pt triplete sau cvartete), iar a
; doua functie lambda are ca parametru tuplul de start si aplica functia anterioara ce aplica transformarile functionale,
; ce se apeleaza utilizand caracteristicile transformarii dorite 
(define get-nth-tuple
  (lambda (n function-type)
    (lambda (start-tuple) (apply-functional-transformations (get-functions-list (get-transformations n) '() function-type) start-tuple))))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.

; folosind n, tripletutl de start si tipul functiei (adica pentru triplete), apelam get-nth-tuple
(define triplet-start '(3 4 5))
(define (get-nth-ppt-from-matrix-transformations n)
  ((get-nth-tuple n function-type-triplets) triplet-start))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.

; folosind n, cvartetul de start si tipul functiei (adica pentru cvartete), apelam get-nth-tuple
(define quadruple-start '(1 1 2 3))
(define (get-nth-quadruple n)
  ((get-nth-tuple n function-type-quadruple) quadruple-start))

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.

; ne folosim de formulele:
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.

; functii care scot g, e, f si h din rezultatul functiei get-nth-quadruple
(define (get-g n)
  (car (get-nth-quadruple n)))

(define (get-e n)
  (cadr (get-nth-quadruple n)))

(define (get-f n)
  (caddr (get-nth-quadruple n)))

(define (get-h n)
  (cadddr (get-nth-quadruple n)))

; functie care aplica formula pentru a obtine tripletul dorit
(define (get-nth-ppt-from-GH-quadruples n)
  (append (list (* (get-g n) (get-h n))) (list (* 2 (get-e n) (get-f n))) (list (+ (* (get-e n) (get-e n)) (* (get-f n) (get-f n))))))
