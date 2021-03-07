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
        ((or(equal? (car list) 12);Identifica si es una J
            (equal? (car list) 13);Identifica si es una Q
            (equal? (car list) 14));Identifica si es una K
            (sumarCartas (cdr list) (+ val 10) As))
        ((equal? (car list) 11) (sumarCartas (cdr list) (+ val 11) #t))
        (else (sumarCartas (cdr list) (+ val (car list)) As))
  ))