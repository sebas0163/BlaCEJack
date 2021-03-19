#lang racket
(require "BlaCEJack.rkt")
(require (lib "graphics.ss" "graphics"))(open-graphics)
#|
Definición del maso inicial del juego
|#
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
; Lista que guardará las cartas y los puntos del jugador1
(define Jugador1 '(()))
; Lista que guardará las cartas y los puntos del jugador2
(define Jugador2 '(()))
; Lista que guardará las cartas y los puntos del jugador3
(define Jugador3 '(()))
; Lista que guardará las cartas y los puntos del crupier
(define crupier '(()))
;Posición inicial para las cartas del jugador 1 y el crupier
(define posx 300)
;Posición inicial para las cartas del jugador 2 y 3.
(define posY 140)
; Variable que indica el turno actual
(define turnos 1)
;Variable que apuntará a la ventana donde todo será dibujado
(define ventana 0)
; Guardará el puntaje obtenido por el jugador, mientras el crupier analiza su baraja.
(define puntajeObtenido 0)
; variable que indica cuantos jugadores se encuentran en la partida
(define jugadores 0)
; Variable que indica si se puede dibujar cartas en la posción del crupier
(define casa #t)
; Variable que guarda el nombre de los jugadores
(define nombres '())

#|
Nombre: bCEj
Autor: Sebastián Moya Monge
Descripción: Función encargada de iniciar el juego, el listener y llama a dibujar el tablero según los jugadores
Input: lista con los nombres de los jugadores
Output: void
|#
(define (bCEj x)(set! ventana (open-viewport "ventana" 1000 700))
  (set! jugadores (length x))(set! nombres x)
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1000 700 "Sea green")
  (cond
    ((= jugadores 1)(unJugador))
    ((= jugadores 2)(dosJugadores))
    ((= jugadores 3)(tresJugadores)))
  (control))
#|
Nombre: unJugador
Autor: Sebastián Moya Monge
Descripción: Función encargada de dibujar la mesa para solo un jugador
Output: void
|#
(define (unJugador)
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 200 20) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 510) "PUNTAJE:")
  ((draw-string ventana)(make-posn 140 580) (~a "JUGADOR 1 : " (car nombres)))
  ((draw-string ventana)(make-posn 220 615) "Pedir")
  ((draw-string ventana)(make-posn 220 665) "dejar")
  ((draw-pixmap ventana)"imagenes/mazo.png"(make-posn 450 250))
  )
#|
Nombre: dosJugadores
Autor: Sebastián Moya Monge
Descripción: Función encargada de dibujar la mesa para solo dos jugador
Output: void
|#
(define (dosJugadores)
  ((draw-solid-rectangle ventana)(make-posn 900 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 200 510) "PUNTAJE:")
  ((draw-string ventana)(make-posn 200 20) "PUNTAJE:")
  ((draw-string ventana)(make-posn 820 35) "PUNTAJE:")
  ((draw-string ventana)(make-posn 140 580) (~a "JUGADOR 1 : " (car nombres)))
  ((draw-string ventana)(make-posn 820 15) (~a "JUGADOR 2 : " (cadr nombres)))
  ((draw-string ventana)(make-posn 220 615) "Pedir")
  ((draw-string ventana)(make-posn 220 665) "dejar")
  ((draw-string ventana)(make-posn 920 65) "Pedir")
  ((draw-string ventana)(make-posn 920 115) "dejar")
  ((draw-pixmap ventana)"imagenes/mazo.png"(make-posn 450 250)))
#|
Nombre: tresJugadores
Autor: Sebastián Moya Monge
Descripción: Función encargada de dibujar la mesa para solo tres jugadores
Output: void
|#
(define (tresJugadores)
  ((draw-solid-rectangle ventana)(make-posn 20 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 20 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 45) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 900 90) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 590) 75 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 200 640) 75 40 "gray")
  ((draw-string ventana)(make-posn 200 80) "CRUPIER")
  ((draw-string ventana)(make-posn 140 580) (~a "JUGADOR 1 : " (car nombres)))
  ((draw-string ventana)(make-posn 820 15) (~a "JUGADOR 2 : " (cadr nombres)))
  ((draw-string ventana)(make-posn 20 15) (~a "JUGADOR 3 : " (caddr nombres)))
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
  ((draw-pixmap ventana)"imagenes/mazo.png"(make-posn 450 250))
  )

#|
Nombre: mostrarPuntaje
Autor: Sebastián Moya Monge
Descripción: Función que muestra en la mesa el puntaje obtenido por un jugador
Input: el puntaje obtenido y el número de jugador 
Output: void
|#
(define (mostrarPuntaje puntaje numJugador)
  (cond
    ((= 1 numJugador)((draw-string ventana)(make-posn 200 530) puntaje))
    ((= 2 numJugador)((draw-string ventana)(make-posn 900 35) puntaje))
    ((= 3 numJugador)((draw-string ventana)(make-posn 100 35) puntaje))
    ((= 4 numJugador)((draw-string ventana)(make-posn 200 40) puntaje))
    ))
#|
Nombre: ganador
Autor: Sebastián Moya Monge
Descripción: Función que muestra una ventana con los resultados del juego
Input: puntaje del crupier
Output: void
|#
(define (ganador crupier)
  (define emergente (open-viewport "Ganador" 500 200))
  ((draw-solid-rectangle emergente)(make-posn 0 0) 500 200 "black")
  
  (cond
    ((or(and(> crupier puntajeObtenido)(<= crupier 21 ))(and(< crupier puntajeObtenido)(> puntajeObtenido 21 )(<= crupier 21)))((draw-string emergente)(make-posn 75 100 ) "La casa gana, el jugador ha perdido" "white"))
    ((or(and (> puntajeObtenido crupier)(<= puntajeObtenido 21))(and(< puntajeObtenido crupier)(> crupier 21 )(<= puntajeObtenido 21)))((draw-string emergente)(make-posn 200 100 ) "¡Felicitaciones has ganado!" "white"))
    (else((draw-string emergente)(make-posn 250 100 ) "EMPATE" "white"))
    )
  (sleep 3)
  (close-viewport emergente)
  (reset))

#|
Nombre: inicio
Autor: Sebastián Moya Monge
Descripción: Función que dibuja las primeras dos cartas del jugador
input: lista del jugador
Output: void
|#
(define (inicio jugador)
  (dibujarCarta (~a(~a(caaar jugador)(cadr(caar jugador)))".png")) (dibujarCarta (~a(~a(caadr(car jugador))(cadr(cadr(car jugador))))".png")))

#|
Nombre: dibujarCarta
Autor: Sebastián Moya Monge
Descripción: Función que dibuja una carta en un lugar específico, segun el jugador
Input: nombre de la imagen que se desea cargar
Output: void
|#

(define (dibujarCarta carta)
  (cond((and(= turnos 1)casa) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn posx 550))(set! posx (+ posx 35)))
       ((and(= turnos 2)casa) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn 875 posY))(set! posY (+ posY 35)))
       ((and(= turnos 3)casa) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn 15 posY))(set! posY (+ posY 35)))
       ((equal? casa #f) ((draw-pixmap ventana)(~a "imagenes/" carta)(make-posn posx 10))(set! posx (+ posx 35)))
  ))
#|
Nombre: revisarCartas
Autor: Sebastián Moya Monge
Descripción: Funcion encargada de enviar a dinujar la carta deseada
Input: lista del jugador en turno
Output: void
|#
(define (revisarCartas jugador)
  (cond((null? (car jugador))(print "no hay cartas"))
        (else(dibujarCarta (~a(~a(caaar jugador)(cadr(caar jugador)))".png")))))

#|
Nombre: masoCrupier
Autor: Sebastián Moya Monge
Descripción: Función que pide las cartas del crupier y las manda a dibujar
Input: maso del crupier y puntaje obtenido por el 
Output: void
|#
(define (masoCrupier maso puntaje)
  (cond
    ((null? maso)(mostrarPuntaje (number->string puntaje) 4)(ganador puntaje)(set! casa #t))
    (else(dibujarCarta (~a(~a(caar maso)(cadr(car maso))) ".png"))(sleep 1)(masoCrupier (cdr maso) puntaje))

  ))
#|
Nombre: control
Autor: Sebastián Moya Monge
Descripción: Función encargada de escuchar los eventos del mouse
Output: void
|#
(define (control)
  (cond((and(> turnos jugadores)(not(= turnos 4)))
        (close-viewport ventana))
        (else (cond
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))95)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))20)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))85)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))40)(= turnos 3))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))95)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))20)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))130)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))90)(= turnos 3))
            (dejar))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))975)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))900)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))85)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))45)(= turnos 2))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))975)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))900)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))130)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))90)(= turnos 2))
            (dejar))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))275)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))200)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))630)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))590)(= turnos 1))
            (pedirCartas))
           ((and (<= (posn-x(mouse-click-posn (get-mouse-click ventana)))275)(>= (posn-x(mouse-click-posn (get-mouse-click ventana)))200)(<= (posn-y(mouse-click-posn (get-mouse-click ventana)))680)(>= (posn-y(mouse-click-posn (get-mouse-click ventana)))640)(= turnos 1))
            (dejar)))
         (control))
         
  ))
#|
Nombre: reset
Autor: Sebastián Moya Monge
Descripción: Función que devuelve los valores a los establecidos en un inicio y dibuja de nuevo la mesa
Output: void
|#
(define (reset)
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1000 700 "Sea green")
  (set! puntajeObtenido 0)
  (set! posx 300)
  (set! crupier '(()))
  (set! cartas '((2 c) (2 p) (2 d) (2 t)
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
  (cond
    ((= jugadores 1)(unJugador))
    ((= jugadores 2)(dosJugadores))
    ((= jugadores 3)(tresJugadores))))

;;;;;;;;;;;;;;;;;; Conexion con la lógica;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Nombre: perdirCartas
Autor: Sebastián Moya Monge
Descripción: Función encargada de solicitar las cartas para cada jugador en específico y enviarlas a dibujar
Output: void
|#
(define (pedirCartas);hacer las llamadas a la lógica 
  (cond
    ((= turnos 1)
     (cond
       ((null? (car Jugador1))(guardarValores(turno (car Jugador1)cartas)1)(inicio Jugador1))
       (else (guardarValores(turno (car Jugador1)cartas) 1)(revisarCartas Jugador1))))
    ((= turnos 2)
     (cond
       ((null? (car Jugador2))(guardarValores(turno (car Jugador2)cartas)2)(inicio Jugador2))
       (else (guardarValores(turno (car Jugador2)cartas)2)(revisarCartas Jugador2))))
    ((= turnos 3)
     (cond
       ((null? (car Jugador3))(guardarValores(turno (car Jugador3)cartas)3)(inicio Jugador3))
       (else(guardarValores(turno (car Jugador3)cartas)3)(revisarCartas Jugador3))))
    ))
       
#|
Nombre: dejar
Autor: Sebastián Moya Monge
Descripción: Función que termina el turno de cada jugador
Output: void
|#
(define (dejar)
  (cond((= turnos 1)(set! turnos 2)(set! casa #f)(set! puntajeObtenido (cadr Jugador1))(mostrarPuntaje (number->string puntajeObtenido)1)(set! posx 300)(guardarValores(houseDeck (car crupier) cartas)4)(masoCrupier (reverse(car crupier)) (cadr crupier)))
       ((= turnos 2)(set! turnos 3)(set! casa #f)(set! puntajeObtenido (cadr Jugador2))(mostrarPuntaje(number->string puntajeObtenido)2)(set! posY 140)(guardarValores(houseDeck (car crupier) cartas)4)(masoCrupier (reverse(car crupier)) (cadr crupier)))
       ((= turnos 3)(set! turnos 5)(set! casa #f)(set! puntajeObtenido (cadr Jugador3))(mostrarPuntaje(number->string puntajeObtenido)3)(set! posY 140)(guardarValores(houseDeck (car crupier) cartas)4)(masoCrupier (reverse(car crupier)) (cadr crupier)))
       ))
#|
Nombre: guardarValores
Autor: Sebastián Moya Monge
Descripción: Función encargada establecer los valores para cada lista en específico
Input: lista otrogada por la lógica y jugador al que le corresponde la lista
Output: void
|#
(define (guardarValores lista numJugador)
  (cond((= numJugador 1) (set! Jugador1 (car lista)))
       ((= numJugador 2) (set! Jugador2 (car lista)))
       ((= numJugador 3) (set! Jugador3 (car lista)))
       ((= numJugador 4) (set! crupier (car lista)))
       )
  (set! cartas (cadr lista))
  )





