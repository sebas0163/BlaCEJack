#lang racket

;revisarAs
;Función que revisa si el valor tiene un as y el resultado de la suma de las cartas
;para verificar si el valor del As es de 1 o de 11
;val: entero que representa el valor de la suma de las cartas
;As: bool que representa si hay o no un As
(define (revisarAs val As)
  (cond ((and (equal? As #t) (> val 21)) (- val 10))
        (else val)
        ))

;sumarCartas
;función que suma las cartas 
;list recibe la lista de cartas 
;val recibe un valor para almacenar la suma  este debe ser 0
;As: recibe un "boolean" para saber si posee un As debe ser #f 
(define (sumarCartas list val As)
  (cond ((null? list) (revisarAs val As))
        ((or(= (caar list) 12);Identifica si es una J
            (= (caar list) 13);Identifica si es una Q
            (= (caar list) 14));Identifica si es una K
            (sumarCartas (cdr list) (+ val 10) As))
        ((equal? (caar list) 11) (sumarCartas (cdr list) (+ val 11) #t))
        (else (sumarCartas (cdr list) (+ val (caar list)) As))
  ))


;DeleteCard
;Funcion que al sacar una carta de la baraja principal, elimina esta carta de la lista que representa a la baraja
;para ajustar las probabilidades de sacar cada carta
;lista: lista a la que se le va a eliminar la carta
;elemento: carta que se desea eliminar
(define (DeleteCard lista elemento)
  (cond
    [(null? lista) empty]
    [(equal? elemento (car lista))(DeleteCard (cdr lista) elemento)]
    [else (cons (car lista) (DeleteCard (cdr lista) elemento))]
    ))


;GetCard
;Funcion obteneque obtiene una carta de la baraja inglesa
;lista: lista de cartas
;num: posición de la carta que se desea obtener
;cont: posicion de la carta que se esta analizando 
(define (GetCard lista num cont)
  (cond ((equal? (length lista) 1) (car lista))
        ((= num (+ cont 1)) (car lista))
        (else (GetCard (cdr lista) num (+ cont 1))))) 



;houseDeck
;Función que realiza el juego del crupier mediante la llamada de la funcion auxiliar
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
(define (houseDeck MyDeck MainDeck)
        (houseDeckAux MyDeck MainDeck (random (length MainDeck))))


;houseDeckAux
;Funcion auxiliar de hoseDeck la cual realiza la jugada del crupier
;Esta obtiene cartas del mazo principal y las agrega al mazo del crupier siempre y
;cuando la suma de las cartas sea menos a 17
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (houseDeckAux MyDeck MainDeck elem)
  (cond ((> (sumarCartas MyDeck 0 #f) 21) (list (list MyDeck (sumarCartas MyDeck 0 #f)) MainDeck))
        ((and (>= (sumarCartas MyDeck 0 #f) 17) (<= (sumarCartas MyDeck 0 #f) 21)) (list (list MyDeck (sumarCartas MyDeck 0 #f)) MainDeck))
        (else (houseDeckAux (cons (GetCard MainDeck elem 0) MyDeck) (DeleteCard MainDeck (GetCard MainDeck elem 0)) (random (length MainDeck))))))

;turno
;Función que realiza el turno de un jugador llamando a su función auxiliar
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
(define (turno myDeck mainDeck)
        (turnoAux myDeck mainDeck (random (length mainDeck))))


;turnoAux
;Función que obtiene una carta del mazo principal y la agrega a su baraja
;en caso de no tener cartas se le agregan dos cartas
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (turnoAux myDeck mainDeck elem)
        (cond ((< (length myDeck) 1) (turnoAux (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))
              (else (list (list (cons (GetCard mainDeck elem 0) myDeck) (sumarCartas (cons (GetCard mainDeck elem 0) myDeck) 0 #f) ) (DeleteCard mainDeck (GetCard mainDeck elem 0)) ))
              ))


;(houseDeck '() '((2 c) (2 p) (2 d) (2 t)(3 c) (3 p) (3 d) (3 t)(4 c) (4 p) (4 d) (4 t)(5 c) (5 p) (5 d) (5 t)(6 c) (6 p) (6 d) (6 t)(7 c) (7 p) (7 d) (7 t)(8 c) (8 p) (8 d) (8 t)(9 c) (9 p) (9 d) (9 t)(10 c) (10 p) (10 d) (10 t)(11 c) (11 p) (11 d) (11 t)(12 c) (12 p) (12 d) (12 t)(13 c) (13 p) (13 d) (13 t)(14 c) (14 p) (14 d) (14 t)))
;(houseDeck '() '((2 c) (2 p) (2 d) (2 t)(3 c) (3 p) (3 d) (3 t)(4 c) (4 p) (4 d) (4 t)(5 c) (5 p) (5 d) (5 t)(6 c) (6 p) (6 d) (6 t)(7 c) (7 p) (7 d) (7 t)(8 c) (8 p) (8 d) (8 t)(9 c) (9 p) (9 d) (9 t)(10 c) (10 p) (10 d) (10 t)(11 c) (11 p) (11 d) (11 t)(12 c) (12 p) (12 d) (12 t)(13 c) (13 p) (13 d) (13 t)(14 C) (14 P) (14 D) (14 T)) (random 52))




;'((2 C) (2 P) (2 D) (2 T)
;  (3 C) (3 P) (3 D) (3 T)
;  (4 C) (4 P) (4 D) (4 T)
;  (5 C) (5 P) (5 D) (5 T)
;  (6 C) (6 P) (6 D) (6 T)
;  (7 C) (7 P) (7 D) (7 T)
;  (8 C) (8 P) (8 D) (8 T)
;  (9 C) (9 P) (9 D) (9 T)
;  (10 C) (10 P) (10 D) (10 T)
;  (11 C) (11 P) (11 D) (11 T)
;  (12 C) (12 P) (12 D) (12 T)
;  (13 C) (13 P) (13 D) (13 T)
;  (14 C) (14 P) (14 D) (14 T))