#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

;; Pornind de la numerele e și f din cvartetele GH, putem
;; obține trei soluții (a1,b1,c1), (a2,b2,c2), (a3,b3,c3)
;; care respectă relația a^2 + b^2 = c^2.
;; Aceste triplete nu reprezintă TPP, și în anumite cazuri
;; conțin coeficienți negativi, însă acest lucru nu ne
;; împiedică să le folosim pentru a crea cheia de
;; criptare/decriptare pentru un criptosistem simplu.

;; Cele trei soluții se obțin conform formulelor de mai jos:
(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

;; Funcția check nu are utilitate pentru temă, este doar
;; un mod de a:
;; - vizualiza tripletele generate pentru 2 numere e și f
;; - testa că ele respectă relația a^2 + b^2 = c^2
(define (check e f)
  (let ((a1 (a1 e f)) (b1 (b1 e f)) (c1 (c1 e f))
                      (a2 (a2 e f)) (b2 (b2 e f)) (c2 (c2 e f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))

;; Exemplu de utilizare
;; (cu primele 3 generații din arborele de cvartete):
;; (map check
;;      '(1 1 2 2 1 4 4 2 5 5 2 3 3)
;;      '(2 4 5 3 6 9 7 9 12 8 7 8 4))

;; Vom trimite mesaje folosind cele 26 de caractere din
;; alfabetul englez (doar în varianta cu literă mică),
;; plus caracterul "spațiu". În consecință, avem nevoie să
;; codificăm 27 de caractere distincte:
;;  - asociem codul 0 caracterului "spațiu"
;;  - asociem codurile 1-26 caracterelor 'a', 'b' ... 'z'
;;
;; Cele două părți angrenate în comunicare își vor trimite un
;; număr n, pe baza căruia fiecare va determina cheia de
;; criptare/decriptare astfel:
;;  - determină al n-lea cvartet din arborele TPP
;;  - extrag valorile e și f din acest cvartet
;;  - cheia va fi (a1,b1,c1,a2,b2,c2,a3,b3,c3) mod 27
;;    (fiecare valoare din tuplu devine un cod între 0 și 26)

; TODO
; Implementați o funcție care primește un număr n și
; obține cheia de criptare/decriptare conform algoritmului
; de mai sus.
; Folosiți o formă adecvată de let pentru a extrage e și f.
; Utilizați o funcțională astfel încât să nu scrieți 9 bucăți 
; de cod foarte similare (de exemplu, numele operației mod ar
; trebui să apară o singură dată).

; extrag g si f din cvartet; formez lista cu (a1 b1 c1 a2 b2 c3 a3 b3 c3); fac modulo 27 din fiecare element al listei
(define (key n)
  (let* ([e (cadr (get-nth-quadruple n))] [f (caddr (get-nth-quadruple n))] [crypt-list (list (a1 e f) (b1 e f) (c1 e f) (a2 e f) (b2 e f) (c2 e f) (a3 e f) (b3 e f) (c3 e f))])
    (map (lambda (x) (modulo x 27)) crypt-list)))

; TODO
; Implementați o funcție care primește un mesaj (un șir de
; caractere care conține doar litere mici și spații) și
; întoarce o listă de coduri asociate mesajului respectiv
; (spațiu devine 0, 'a' devine 1 ... 'z' devine 26).
; Funcții utile: string->list, char->integer

; din string -> in lista de int
; in map-ul din interior transform fiecare caracter din lista de caractere in integer; in map-ul exterior fac modulo 32 din fiecare element al noii liste de integeri
; in ascii, space -> 32; a-> 97; b->98 ... ; facand modulo 32 din fiecare, obtin fix ordinea de la 0 la 26 pe care o vreau
(define (message->codes message)
  (map (lambda (x) (modulo x 32))(map char->integer (string->list message))))

; TODO
; Implementați o funcție care primește o listă de coduri
; (numere între 0 și 26) și întoarce mesajul asociat
; (procesul invers celui de la funcția message->codes).
; Funcții utile: list->string, integer->char

; din lista de int -> in string
; in map-ul interior aduc integerii la codul ascii corespunzator; apoi, transfom fiecare element din lista de
; integeri in char-uri folosind map-ul exterior; la final, rezultatul il transform din lista de char-uri
; in string
(define (codes->message codes)
  (list->string (map integer->char (map (lambda (x) (if (equal? x 0) 32 (+ x 96))) codes))))

;; Pentru operațiile de criptare și decriptare, lucrăm
;; cu coduri între 0 și 26.
;; Folosim următoarele notații:
;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii
;;
;; Pentru a putea efectua operații de criptare/decriptare,
;; cheia trebuie extinsă/trunchiată la dimensiunea
;; mesajului care trebuie criptat/decriptat.
;;
;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)          
;;
;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)

; TODO
; Implementați o funcție care primește o cheie key și o
; dimensiune size și extinde/trunchiază cheia key la
; dimensiunea size.
; Folosiți cel puțin o formă de let.

; size / length(key) reprezinta append-urile intregi ale cheii la rezultat (care initial e lista vida
; folosesc let pentru a apela recursiv
; la fiecare recursivitate scad 1 din full append-uri si adaug cheia la rezultat
; la final (cand am facut nr total de append-uri), adaug (size % length(key)) elemente din key la rezultat
(define (extend-key key size)
  (let redimension_key ([total_full_appends (exact-floor(/ size (length key)))] [result '()])
    (if (zero? total_full_appends)
        (append result (take key (modulo size (length key))))
        (redimension_key (sub1 total_full_appends) (append result key)))))

; TODO
; Observați că algoritmii de criptare/decriptare se
; aseamănă foarte mult. Abstractizați procesul într-o
; funcție mai generală (încercați să îi dați un nume
; sugestiv) din care derivați apoi, cu minim de efort,
; funcțiile:
;    (encrypt-codes message key)
;    (decrypt-codes crypted key)
; Nu folosiți recursivitate explicită (ci funcționale).

; Ambele funcții primesc două liste de coduri (reprezentând
; mesajul clar/criptat, respectiv cheia) și întorc o listă
; de coduri (mesajul criptat/decriptat, după caz).

; functii pentru operatia de criptare/ decriptare, conform algoritmului
(define (encrypt-function m k)
  (modulo (+ m k) 27))

(define (decrypt-function c k)
  (modulo (- c k) 27))

; functie curry comuna de procesare
; se primeste in prima functie lambda codul de criptat/decriptat si cheia
; extind cheia -> cu let, folosesc extended-key ca fiind rezultatul extinderii
; in urmatoarea functie lambda, primesc functia de care am nevoie (de criptare/decriptare)
; si cu map aplic functia pe code si extended-key
(define cryptsystem-processing
  (lambda (code key)
    (let ((extended-key (extend-key key (length code))))
      (lambda (processing-function)
        (map processing-function code extended-key)))))

; aplic functia comuna pentru cazurile de criptare si decriptare
(define (encrypt-codes message key)
  ((cryptsystem-processing message key) encrypt-function))

(define (decrypt-codes crypted key)
  ((cryptsystem-processing crypted key) decrypt-function))
    
; TODO
; Analog funcțiilor encrypt-codes și decrypt-codes care
; operează pe coduri, implementați funcțiile encrypt-message
; și decrypt-message care operează pe șiruri de caractere.
; În acest caz, ambele funcții primesc un șir de caractere
; (mesajul clar/criptat) și o listă de coduri (cheia) și
; întorc un șir de caractere (mesajul criptat/decriptat).

; transform string-ul in lista de int-uri, criptez/decriptez  utilizand
; functiile precedente, apoi rezultatul il transform din lista de int-uri
; in string
(define (encrypt-message message key)
  (codes->message (encrypt-codes (message->codes message) key)))
(define (decrypt-message crypted key)
  (codes->message (decrypt-codes (message->codes crypted) key)))
           