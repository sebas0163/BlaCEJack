#lang racket

;No es util :c 
(define (randomCart list)
  (cond ((equal? (null? (cdr list)) #t) (car list))
        ((equal? (<= (random) '0.2) #t) (car list))
        (else (randomCart (cdr list)))
        ))
;(randomCart '(11 8 3 10 6 1 7 14 12 5 9 4 13 2 9))
;(randomCart '(11 8 3 10 6 1 7 14 12 5 9 4 13 2 9))


;Función que revisa si el valor tiene un as y el resultado de la suma de las cartas
;para verificar si el valor del As es de 1 o de 11
(define (revisarAs val As)
  (cond ((and (equal? As #t) (> val 21)) (- val 10))
        (else val)
        ))

; función que suma las cartas XD
; recibe la lista de cartas (list)
; recibe un valor para almacenar la suma (val) este debe ser 0
; recibe un "boolean" para saber si posee un As (As) debe ser #f 
(define (sumarCartas list val As)
  (cond ((null? list) (revisarAs val As))
        ((or(= (caar list) 12);Identifica si es una J
            (= (caar list) 13);Identifica si es una Q
            (= (caar list) 14));Identifica si es una K
            (sumarCartas (cdr list) (+ val 10) As))
        ((equal? (caar list) 11) (sumarCartas (cdr list) (+ val 11) #t))
        (else (sumarCartas (cdr list) (+ val (caar list)) As))
  ))




;Funcion que al sacar una carta de la baraja principal, elimina esta carta de la lista que representa a la baraja
;para ajustar las probabilidades de sacar cada carta
(define (DeleteCard lista elemento)
  (cond
    [(null? lista) empty]
    [(equal? elemento (caar lista))(DeleteCard (cdr lista) elemento)]
    [else (cons (car lista) (DeleteCard (cdr lista) elemento))]
    ))

;Funcion obtener carta de la baraja inglesa
(define (GetCard lista num cont)
  (cond ((null? (cdr lista)) (car list))
        ((= num (+ cont 1)) (car lista))
        (else (GetCard (cdr lista) num (+ cont 1))))) 

;funcion que administra el mazo del jugador/crupier
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal

;my deck value se podria ignorar* 
(define (houseDeck MyDeck MainDeck elem)
  (cond ((> (sumarCartas MyDeck 0 #f) 21) (list (sumarCartas MyDeck 0 #f) (mostrarCartas MyDeck) ))
        ((and (>= (sumarCartas MyDeck 0 #f) 17) (<= (sumarCartas MyDeck 0 #f) 21)) (list (list (sumarCartas MyDeck 0 #f) (mostrarCartas MyDeck))))
        (else (houseDeck (cons (GetCard MainDeck elem 0) MyDeck) (DeleteCard MainDeck (GetCard MainDeck elem 0)) (random (length MainDeck))))))


;Función que obtiene una lista y cambia los valores 11,12 13 y 14 por A,J,Q y K
;list lista de las cartas de un jugador
(define (mostrarCartas list)
  (cond ((null? list) '())
        ((=(caar list) 11) (cons 'A (mostrarCartas (cdr list))))
        ((=(caar list) 12) (cons 'J (mostrarCartas (cdr list))))
        ((=(caar list) 13) (cons 'Q (mostrarCartas (cdr list))))
        ((=(caar list) 14) (cons 'K (mostrarCartas (cdr list))))
        (else (cons (caar list) (mostrarCartas (cdr list))))
   ))

;Función que obtiene una lista de la suma de la suma de las cartas de cada jugador (la mesa primero) y dice quien fue el ganador
;listValJug: lista con la suma de las cartas de los jugadores
;valMay: el valor mayor encontrado y menor a 21 (inicialmente es 0)
;jugGanador: int que especifica cual es el jugador que quedo de primero, en caso de haber empate se convierte en una lista (empieza en 0)
;jugAnalizado: contador para saber cual jugador se esta analizando (empieza en 1)
(define (ganador listValJug valMay jugGanador jugAnalizado)
  (cond ((and (null? listValJug) (list? jugGanador)) (list "empate de los jugadores" jugGanador))
        ((and (null? listValJug) (= jugGanador 1)) "La casa gana")
        ((null? listValJug) (list "El ganador es el jugador" jugGanador))
        ((and (> (car listValJug) valMay) (<= (car listValJug) 21)) (ganador (cdr listValJug) (car listValJug) jugAnalizado (+ jugAnalizado 1)))
        ((= (car listValJug) valMay) (ganador (cdr listValJug) (car listValJug) (list jugGanador jugAnalizado)  (+ jugAnalizado 1)))
        (else (ganador (cdr listValJug) (car listValJug) jugGanador (+ jugAnalizado 1)))
        ))

;(ganador '( 16 17 17 21) 0 0 1)

;Función que pregunta al usuario si quiere continuar jugando
(define (seguirJugando?)
  (cond ((equal? (read-line) "s") #t)
        (else #f)
        ))

;Función para empezar el juego con 1 jugador
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (unJugador myDeck mainDeck elem)
  (cond ((<(length myDeck) 2) (unJugador (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))
        ((or (equal? (seguirJugando?) #f) (> (sumarCartas myDeck 0 #f) 21)) (cons (list(sumarCartas myDeck 0 #f) myDeck) (houseDeck '() mainDeck (random (length mainDeck)))))
        (else  (unJugador (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))))

;Función para empezar el juego con 2 jugadores
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (dosJugadores myDeck mainDeck elem)
  (cond ((<(length myDeck) 2) (dosJugadores (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))
        ((or (equal? (seguirJugando?) #f) (> (sumarCartas myDeck 0 #f) 21)) (cons (list(sumarCartas myDeck 0 #f) myDeck) (unJugador '() mainDeck (random (length mainDeck)))))
        (else  (dosJugadores (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))))

;Función para empezar el juego con 3 jugadores
;MyDeck: cartas en mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (tresJugadores myDeck mainDeck elem)
  (cond ((<(length myDeck) 2) (tresJugadores (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))
        ((or (equal? (seguirJugando?) #f) (> (sumarCartas myDeck 0 #f) 21)) (cons (list(sumarCartas myDeck 0 #f) myDeck) (dosJugadores '() mainDeck (random (length mainDeck)))))
        (else  (tresJugadores (cons (GetCard mainDeck elem 0) myDeck) (DeleteCard mainDeck (GetCard mainDeck elem 0)) (random (length mainDeck))))))

(define (jugar list)
  (cond ((= (length list) 1) (unJugador '() '((2 C)  (2 P) 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14 14 14 14) (random 52)))
        ((= (length list) 2) (dosJugadores '() '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14 14 14 14) (random 52)))
        ((= (length list) 3) (tresJugadores '() '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14 14 14 14) (random 52)))
        ))

;(tresJugadores '() '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14 14 14 14) (random 52))
;(jugar '("pedro" "juan" "esteban"))

;'((2 C) (2 P) (2 D) (2 T)
;    (3 C) (3 P) (3 D) (3 T)
;    (4 C) (4 P) (4 D) (4 T)
;    (5 C) (5 P) (5 D) (5 T)
;    (6 C) (6 P) (6 D) (6 T)
;    (7 C) (7 P) (7 D) (7 T)
;    (8 C) (8 P) (8 D) (8 T)
;    (9 C) (9 P) (9 D) (9 T)
;    (10 C) (10 P) (10 D) (10 T)
;    (11 C) (11 P) (11 D) (11 T)
;    (12 C) (12 P) (12 D) (12 T)
;    (13 C) (13 P) (13 D) (13 T)
;    (14 C) (14 P) (14 D) (14 T))