#lang racket
(require (lib "graphics.ss" "graphics"))(open-graphics)
(define cartas '((2 c) (2 p) (2 d) (2 t)
                 (3 c) (3 p) (3 d) (3 t)
                 (4 c) (4 p) (4 d) (4 t)
                 (5 c) (5 p) (5 d) (5 t)
                 (6 c) (6 p) (6 d) (6 t)
                 (7 c) (7 p) (7 d) (7 t)
                 (8 c) (8 p) (8 d) (8 t)
                 (9 c) (9 p) (9 d) (9 t)
                 (10 c) (10 p) (10 d) (10 t)
                 (11 c) (11 p) (11 d) (11 t)
                 (12 c) (12 p) (12 d) (12 t)
                 (13 c) (13 p) (13 d) (13 t)
                 (14 c) (14 p) (14 d) (14 t)))
(define Jugador1 '(((5 c)(9 p))20))
(define Jugador2 '(((3 t)(5 c)(9 p))20))
(define Jugador3 '(((3 t)(5 c)(9 p))20))
(define crupier '(((3 t)(5 c)(9 p))20))
(define posx 300)
(define posY 140)
(define turno 3)
(define ventana 0)
(define puntajeObtenido 0)

; Inicio de la ejecución
(define (bCEj x)(set! ventana (open-viewport "ventana" 1000 700))
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1000 700 "Sea green")
  (cond
    ((= x 1)(unJugador))
    ((= x 2)(dosJugadores))
    ((= x 3)(tresJugadores))))
; Función que dibuja la mesa para un jugador
; Parametro view-port de la pantalla principal
(define (unJugador)
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 200 20) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 510) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 580) "JUGADOR 1")
  ((draw-string ventana)(make-posn 220 615) "Pedir")
  ((draw-string ventana)(make-posn 220 665) "dejar")
  ;(dibujarCarta 300 400 ventana "p.png")
  ;(dibujarCarta 345 400 ventana "p.png")
  ;(revisarCartas Jugador2 ventana)
  )
; Función que dibuja la mesa para dos jugador
; Parametro view-port de la pantalla principal
(define (dosJugadores)
  ((draw-solid-rectangle ventana)(make-posn 900 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 200 510) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 20) "PUNTAJE:")
  ((draw-string ventana)(make-posn 820 35) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 580) "JUGADOR 1")
  ((draw-string ventana)(make-posn 900 15) "JUGADOR 2")
  ((draw-string ventana)(make-posn 220 615) "Pedir")
  ((draw-string ventana)(make-posn 220 665) "dejar")
  ((draw-string ventana)(make-posn 920 65) "Pedir")
  ((draw-string ventana)(make-posn 920 115) "dejar"))
; Función que dibuja la mesa para tres
(define (tresJugadores)
  ((draw-solid-rectangle ventana)(make-posn 20 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 20 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 200 580) "JUGADOR 1")
  ((draw-string ventana)(make-posn 900 15) "JUGADOR 2")
  ((draw-string ventana)(make-posn 20 15) "JUGADOR 3")
  ((draw-string ventana)(make-posn 200 510) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 20) "PUNTAJE:")
  ((draw-string ventana)(make-posn 820 35) "PUNTAJE:")
  ((draw-string ventana)(make-posn 5 35) "PUNTAJE:")
  ((draw-string ventana)(make-posn 220 615) "Pedir")
  ((draw-string ventana)(make-posn 220 665) "dejar")
  ((draw-string ventana)(make-posn 920 65) "Pedir")
  ((draw-string ventana)(make-posn 920 115) "dejar")
  ((draw-string ventana)(make-posn 40 65) "Pedir")
  ((draw-string ventana)(make-posn 40 115) "dejar")
  #|(mostrarPuntaje "100" ventana 1)
  (mostrarPuntaje "100" ventana 2)
  (mostrarPuntaje "100" ventana 3)
  (mostrarPuntaje "100" ventana 4)|#
  ;(revisarCartas Jugador2 ventana)
;(revisarCartas Jugador2 ventana)
  ;(pedirCartas ventana )
  ;(pedirCartas ventana )
  (inicio Jugador1)
  (prueba)
  )

; Funcion que dibuja el puntaje obtenido por cada jugador
(define (mostrarPuntaje puntaje numJugador)
  (cond
    ((= 1 numJugador)((draw-string ventana)(make-posn 200 530) puntaje))
    ((= 2 numJugador)((draw-string ventana)(make-posn 900 35) puntaje))
    ((= 3 numJugador)((draw-string ventana)(make-posn 100 35) puntaje))
    ((= 4 numJugador)((draw-string ventana)(make-posn 200 40) puntaje)(set! puntajeObtenido 0))
    ))
; Funcion que lanza un cuadro de dialogo al finalizar la partida 
(define (ganador jugador crupier)
  (define emergente (open-viewport "Ganador" 500 200))
  ((draw-solid-rectangle emergente)(make-posn 0 0) 500 200 "black")
  
  (cond
    ((or(and(> crupier jugador)(<= crupier 21 ))(and(< crupier jugador)(<= crupier 21 )))((draw-string emergente)(make-posn 75 100 ) "La casa gana, el jugador ha perdido" "white"))
    ((or(and (> jugador crupier)(<= jugador 21))(and(< jugador crupier)(<= jugador 21 )))((draw-string emergente)(make-posn 200 100 ) "¡Felicitaciones has ganado!" "white"))
    (else((draw-string emergente)(make-posn 250 100 ) "EMPATE" "white"))
    ))

;EVENTOS CON EL MOUSE
;DESACTIVAR "BOTONES

;Funcion que dibuja las primeras dos cartas
(define (inicio jugador)
  (dibujarCarta (~a(~a(caaar jugador)(cadr(caar jugador)))".png")) (dibujarCarta (~a(~a(caadr(car jugador))(cadr(cadr(car jugador))))".png")))

; Función que dibuja la carta en la posición deseada

(define (dibujarCarta carta)
  (cond((= turno 1) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn posx 550))(set! posx (+ posx 35)))
       ((= turno 2) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn 875 posY))(set! posY (+ posY 35)))
       ((= turno 3) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn 15 posY))(set! posY (+ posY 35)))
       (else ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn posx 10))(set! posx (+ posx 35)))
  ))
; Función que revisa la ultima carta agregada al maso del jugador 
(define (revisarCartas jugador)
  (cond((null? (car jugador))(print "no hay cartas"))
        (else(dibujarCarta (~a(~a(caar jugador)(cadr(car jugador)))".png")))))

; Función que dibuja las cartas del crupier
(define (masoCrupier maso puntaje)
  (cond
    ((null? maso)(mostrarPuntaje (number->string puntaje) 4))
    (else(dibujarCarta (~a(~a(caar maso)(cadr(car maso))) ".png"))(sleep 1)(masoCrupier (cdr maso) puntaje))

  ))
;;;;;;;;;;;;;;;;;;; EVENTOS DEL MOUSE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prueba)
         (cond
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))95)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))20)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))85)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))40)(= turno 3))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))95)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))20)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))130)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))90)(= turno 3))
            (dejar))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))975)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))900)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))85)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))45)(= turno 2))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))975)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))900)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))130)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))90)(= turno 2))
            (print "dejar j2"))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))275)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))200)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))630)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))590)(= turno 1))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))275)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))200)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))680)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))640)(= turno 1))
            (print "dejar j1")))
         (prueba)
  )

;;;;;;;;;;;;;;;;;; Conexion con la lógica;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pedirCartas);hacer las llamadas a la lógica 
  (cond
    ((= turno 1)
     (cond
       ((null? (car Jugador1))#|agregar llamada a lógica|#(inicio Jugador1))
       (else (set! Jugador1 (cons '(2 t)(car Jugador1) ))(revisarCartas Jugador1))))
    ((= turno 2)
     (cond
       ((null? (car Jugador2))(inicio Jugador2 ventana))
       (else (set! Jugador2 (cons '(2 t)(car Jugador2) ))(revisarCartas Jugador2))))
    ((= turno 3)
     (cond
       ((null? (car Jugador3))(inicio Jugador3 ventana))
       (else(set! Jugador3 (cons '(2 t)(car Jugador3) ))(revisarCartas Jugador3))))
    ))
       
; Función que acaba con el turno
(define (dejar)
  (cond((= turno 1)(set! turno 4)(set! puntajeObtenido (cadr Jugador1))(mostrarPuntaje (number->string puntajeObtenido)1)(set! posx 300)(masoCrupier (car Jugador2) 20)(set! turno 2))
       ((= turno 2)(set! turno 4)(set! puntajeObtenido (cadr Jugador2))(mostrarPuntaje(number->string puntajeObtenido)2)(set! posY 140)(masoCrupier (car Jugador2) 20)(set! turno 3))
       ((= turno 3)(set! turno 4)(set! puntajeObtenido (cadr Jugador3))(mostrarPuntaje(number->string puntajeObtenido)3)(set! posY 140)(masoCrupier (car Jugador2) 20))
       ))



(bCEj 3)

