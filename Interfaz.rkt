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
(define Jugador1 '((p)))
(define Jugador2 '(((3 t)(5 c)(9 p))20))
(define Jugador3 '(()))

; Inicio de la ejecución
(define (bCEj x)(define ventana (open-viewport "ventana" 1000 700))
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1000 700 "Sea green")
  (cond
    ((= x 1)(unJugador ventana))
    ((= x 2)(dosJugadores ventana))
    ((= x 3)(tresJugadores ventana))))
; Función que dibuja la mesa para un jugador
; Parametro view-port de la pantalla principal
(define (unJugador ventana)
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
  (revisarCartas Jugador2 ventana))
; Función que dibuja la mesa para dos jugador
; Parametro view-port de la pantalla principal
(define (dosJugadores ventana)
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
(define (tresJugadores ventana)
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
  (mostrarPuntaje "100" ventana 4)|#)

; Funcion que dibuja el puntaje obtenido por cada jugador
(define (mostrarPuntaje puntaje ventana numJugador)
  (cond
    ((= 1 numJugador)((draw-string ventana)(make-posn 200 530) puntaje))
    ((= 2 numJugador)((draw-string ventana)(make-posn 900 35) puntaje))
    ((= 3 numJugador)((draw-string ventana)(make-posn 100 35) puntaje))
    ((= 4 numJugador)((draw-string ventana)(make-posn 200 40) puntaje))
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
;DESACTIVAR "BOTONES"
;Mejor método de aumentar posiciones 

; Función que dibuja la carta en la posición deseada

(define (dibujarCarta posX posY ventana carta) ;especificar cada cuanto se corre
  ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn posX posY)))
; Función que revisa la ultima carta agregada al maso del jugador 
(define (revisarCartas jugador ventana)
  (cond((null? (car jugador))(print "no hay cartas"))
        (else(dibujarCarta 300 550 ventana (~a(~a(caaar jugador)(cadr(caar jugador)))".png")))))

;;;;;;;;;;;;;;;;;; Conexion con la lógica;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pedirCartas jugador ventana)
  (revisarCartas jugador ventana))
; Función que acaba con el turno
(define (dejar jugador ventana)(print "El puntaje obtenido es de "))

(bCEj 3)