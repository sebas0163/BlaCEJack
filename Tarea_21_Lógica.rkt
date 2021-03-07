#lang racket

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
  (cond ((null? lista) 0)
        ((= num (+ cont 1)) (car lista))
        (else (GetCard (cdr lista) num (+ cont 1)))))


;Funcion para obtener el valor del mazo de la baraja del jugador
(define (GetDeckValue lista Value)
  (cond ((null? lista) Value)
        (else (GetDeckValue (cdr lista) (+ Value (car lista))))))

;funcion que administra el mazo del jugador/crupier
;MyDeck: cartas en mi mazo
;MyDeckValue: Valor numerico de mi mazo
;MainDeck: Baraja principal, o sea, es la baraja inglesa
;elem: numero de cartas restantes en la baraja principal
(define (PlayerDeck MyDeck MyDeckValue MainDeck elem)
  (cond ((> MyDeckValue 21) "has perdido")
        ((and (>= MyDeckValue 17) (<= MyDeckValue 21)) MyDeckValue)
        (else (PlayerDeck (cons (GetCard MainDeck elem 0) MyDeck) (GetDeckValue MyDeck 0) (DeleteCard MainDeck (GetCard MainDeck elem 0)) (random (- (length MainDeck) 1))))))


;Baraja inglesa
;'(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 (1 11) (1 11) (1 11) (1 11))

;Pruebas
;Prueba funcion GetCard
;(GetCard '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 (1 11) (1 11) (1 11) (1 11)) (random 53) 0)

;(PlayerDeck '() 0 '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10) (random 49))