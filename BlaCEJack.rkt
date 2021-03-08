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
        ((or(= (car list) 12);Identifica si es una J
            (= (car list) 13);Identifica si es una Q
            (= (car list) 14));Identifica si es una K
            (sumarCartas (cdr list) (+ val 10) As))
        ((equal? (car list) 11) (sumarCartas (cdr list) (+ val 11) #t))
        (else (sumarCartas (cdr list) (+ val (car list)) As))
  ))




;Funcion que al sacar una carta de la baraja principal, elimina esta carta de la lista que representa a la baraja
;para ajustar las probabilidades de sacar cada carta
(define (DeleteCard lista elemento)
  (cond
    [(null? lista) empty]
    [(equal? elemento (car lista))(DeleteCard (cdr lista) elemento)]
    [else (cons (car lista) (DeleteCard (cdr lista) elemento))]
    ))

;Funcion obtener carta de la baraja inglesa
(define (GetCard lista num cont)
  (cond ((null? (cdr lista)) (car list))
        ((= num (+ cont 1)) (car lista))
        (else (GetCard (cdr lista) num (+ cont 1))))) 

;funcion que administra el mazo del jugador/crupier
;MyDeck: cartas en mi mazo
;MyDeckValue: Valor numerico de mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal

;my deck value se podria ignorar* 
(define (PlayerDeck MyDeck MyDeckValue MainDeck elem)
  (cond ((> (sumarCartas MyDeck 0 #f) 21) (list (sumarCartas MyDeck 0 #f) (mostrarCartas MyDeck) "has perdido"))
        ((and (>= (sumarCartas MyDeck 0 #f) 17) (<= (sumarCartas MyDeck 0 #f) 21)) (list (sumarCartas MyDeck 0 #f) (mostrarCartas MyDeck)))
        (else (PlayerDeck (cons (GetCard MainDeck elem 0) MyDeck) (sumarCartas MyDeck 0 #f) (DeleteCard MainDeck (GetCard MainDeck elem 0)) (random (length MainDeck))))))


;Función que obtiene una lista y cambia los valores 11,12 13 y 14 por A,J,Q y K
;list lista de las cartas de un jugador
(define (mostrarCartas list)
  (cond ((null? list) '())
        ((=(car list) 11) (cons 'A (mostrarCartas (cdr list))))
        ((=(car list) 12) (cons 'J (mostrarCartas (cdr list))))
        ((=(car list) 13) (cons 'Q (mostrarCartas (cdr list))))
        ((=(car list) 14) (cons 'K (mostrarCartas (cdr list))))
        (else (cons (car list) (mostrarCartas (cdr list))))
   ))

        
(PlayerDeck '() 0 '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14 14 14 14) (random 52))


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

(ganador '( 16 17 17 21) 0 0 1)