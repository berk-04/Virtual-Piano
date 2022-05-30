#lang racket

#|Abrimos la libreria de graficos|#

(require(lib "graphics.ss""graphics"))
(open-graphics)

(require 2htdp/image
(only-in racket/gui/base play-sound))
(require 2htdp/universe)

#|Pregunta inicio|#

(define (pregunta)
  (displayln "\t\n !Welcome to Virtual Piano!")
  (displayln "Do you want to play with mouse or keyboard?")
  (displayln "if you want to play with mouse check 1.")
  (display "if you want to play with keyboard check 2: ")
  (define modo (read))
  #|Pantalla inicio|#
  (define inicio(open-viewport "Piano Virtual" 1200 600))  ;Define una función para abrir la ventana de inicio
  ((draw-pixmap inicio)"Cartel_de_bienvenida.png" (make-posn 0 0))  ;Muestra una foto del tamaño de la ventana (pantalla inicio)
  (animacion_carga inicio 300 310);Llamado a la función de animación de carga en la pantalla de inicio
  #|Ventana principal|#
  (define main(open-viewport "Piano Virtual" 1200 600))
  (Dibujar_piano main 1 1 modo "Pantalla_cancion_1.png")
  )

#|Dibujar piano|#

(define (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)
  (define imagen_nada "Imagen_nada.jpeg")
  (define imagen_notas "Imagen_notas.jpeg")
  (define imagen_letras_y_numeros "Imagen_letras_y_numeros.jpeg") 
  ((draw-viewport main)"gray")
  ((draw-pixmap main)"Base_piano.jpg" (make-posn 0 250))                      ;Definimos una función que dibuja la estructura del piano
  ((draw-pixmap main)pantalla_canciones(make-posn 330 260))
  ((draw-pixmap main)"Boton_auto.png"(make-posn 720 270))
  ((draw-pixmap main)"TYPE.png"(make-posn 152.5 270))
  ((draw-pixmap main)"game.jpg"(make-posn 820 270))
  ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
  ((draw-solid-ellipse main)(make-posn 145 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 220 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 370 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 445 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 520 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 670 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 745 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 895 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 970 355)10 10 "white")
  ((draw-solid-ellipse main)(make-posn 1045 355)10 10 "white")
  (cond
    ((= contador_posiciones 1)
     (if (= modo 1)
         [begin
           ((draw-pixmap main)imagen_nada (make-posn 74 373)) 
           (obtenerclick main imagen_nada contador_posiciones contador_click_canciones modo pantalla_canciones)]
         [begin
           ((draw-pixmap main)imagen_nada (make-posn 74 373))
           (teclado main (key-value (get-key-press main))imagen_nada contador_posiciones contador_click_canciones modo pantalla_canciones)]
         ))
    ((= contador_posiciones 2)
     (if (= modo 1)
         [begin
           ((draw-pixmap main)imagen_notas (make-posn 74 373)) 
           (obtenerclick main imagen_notas contador_posiciones contador_click_canciones modo pantalla_canciones)]
         [begin
           ((draw-pixmap main)imagen_notas (make-posn 74 373))
           (teclado main (key-value (get-key-press main))imagen_notas contador_posiciones contador_click_canciones modo pantalla_canciones)]
         ))
    ((= contador_posiciones 3)
     (if (= modo 1)
         [begin
           ((draw-pixmap main)imagen_letras_y_numeros (make-posn 74 373)) 
           (obtenerclick main imagen_letras_y_numeros contador_posiciones contador_click_canciones modo pantalla_canciones)]
         [begin
           ((draw-pixmap main)imagen_letras_y_numeros (make-posn 74 373))
           (teclado main (key-value (get-key-press main))imagen_letras_y_numeros contador_posiciones contador_click_canciones modo pantalla_canciones)]
         ))
    ((= contador_posiciones 4)
     (if (= modo 1)
         [begin
           ((draw-pixmap main)imagen_nada (make-posn 74 373)) 
           (obtenerclick main imagen_nada 1 contador_click_canciones modo pantalla_canciones)]
         [begin
           ((draw-pixmap main)imagen_nada (make-posn 74 373))
           (teclado main (key-value (get-key-press main))imagen_nada 1 contador_click_canciones modo pantalla_canciones)]
         ))
    ((= contador_click_canciones 3)
     (Dibujar_piano main contador_posiciones 1 modo pantalla_canciones))
    ))

#|Animación de carga|#

(define (animacion_carga inicio xblanca xnegra) ;funcion con argumento de ubicación x para teclas blancas y negras.
  (cond ((< xblanca 830)                 ;Condición de para de la función.
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 1) 510)15 50 "white") ;Primer tecla blanca. Suma 1 para poder mostrar la linea de separación entre teclas.
         (sleep 0.1)                                                       ;Temporizador para dar efecto de barra de carga
         ((draw-solid-rectangle inicio)(make-posn xnegra 510)10 30 "black")  ;Primer tecla negra
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 15) 510)15 50 "white"); En cada parámetro de la función se va sumando de a 15 sin afectar el valor incial
         ((draw-solid-rectangle inicio)(make-posn xnegra 510)10 30 "black")
         ((draw-line inicio)(make-posn (+ xblanca 15) 510)(make-posn (+ xblanca 15) 560));Esta linea da contorno a las teclas
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 15) 510)10 30 "black");Vuelve a dibujar la tecla negra que se borra al dibujar la tecla blanca
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 30) 510)15 50 "white")
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 15) 510)10 30 "black")
         ((draw-line inicio)(make-posn (+ xblanca 30) 510)(make-posn (+ xblanca 30) 560))
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 45) 510)15 50 "white")
         ((draw-line inicio)(make-posn (+ xblanca 45) 510)(make-posn (+ xblanca 45) 560)"black")
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 45) 510)10 30 "black")
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 60) 510)15 50 "white")
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 45) 510)10 30 "black")
         ((draw-line inicio)(make-posn (+ xblanca 60) 510)(make-posn (+ xblanca 60) 560))
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 60) 510)10 30 "black")
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 75) 510)15 50 "white")
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 60) 510)10 30 "black")
         ((draw-line inicio)(make-posn (+ xblanca 75) 510)(make-posn (+ xblanca 75) 560)"black")
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 75) 510)10 30 "black")
         (sleep 0.1)
         ((draw-solid-rectangle inicio)(make-posn (+ xblanca 90) 510)15 50 "white")
         ((draw-solid-rectangle inicio)(make-posn (+ xnegra 75) 510)10 30 "black")
         ((draw-line inicio)(make-posn (+ xblanca 90) 510)(make-posn (+ xblanca 90) 560))
         (sleep 0.1)
         ((draw-line inicio)(make-posn (+ xblanca 105)510)(make-posn (+ xblanca 105)560)"black")
         (animacion_carga inicio (+ xblanca 105) (+ xnegra 105));Llamado recursivo, añadiendo el acumulado de cada 15 que se sumó a las coordenadas
         (sleep 1)            ;Temporizador para cerrar la ventana luego de que carga.
         (close-viewport inicio);Cierra la ventana 
         ;(llama ventana donde está el piano virtual);----Abre la nueva ventana (piano virtual) despues de la carga----
         );Cierra la condición
        );Termina el condicional y a su vez el proceso
  );Cierre de la función definida

;En esta función se tiene en cuenta que la octava de piano tiene un patrón, tal patrón es el que se da en toda la función
;el llamado recursivo, empieza a dibujar otra octava exactamente igual a la primera pero desde la posición en que quedó el borde
;de la última tecla

#|Canciones automáticas|#
;-----------------------
; CANCIÓN 1: La lechuza
;-----------------------

(define (cancion1 main tipo_de_teclado)
[begin
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "LA1Tecla_blanca.wav" #t)                         ;Tecla blanca 6
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "LA1Tecla_blanca.wav" #t)                         ;Tecla blanca 6
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.6)
  (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
  ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.7)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.6)
  (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
  ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.9)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.2)
  (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 750 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.2)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.1)
  (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
  ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.1)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.6)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.2)
  (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 750 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.2)
  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.1)
  (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
  ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.1)
  (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
  ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.9)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.5)
  (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  (sleep 0.4)
  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
  ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
  ])

;-----------------------
;  CANCIÓN 2: Titanic
;-----------------------

(define (cancion2 main tipo_de_teclado)
  [begin
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.9)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.3)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.6)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.4)
    (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
    ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.7)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 1.1)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.4)
    (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
    ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.8)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 1.1)
    (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.6)
    (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
    ((draw-pixmap main)"Tecla_gris2.png"(make-posn 750 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 1.1)
    (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 1.1)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.9)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.3)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.6)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.4)
    (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
    ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.7)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 1.1)
    (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
    ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    (sleep 0.5)
    (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
    ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
    ])

#|Juego|#

(define (cancion_juego_1 main modo)
  [begin
    ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
    (define posicion_click_mouse (get-mouse-click main))
    (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                    (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
               (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                    (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
           [begin
             (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
             ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
             ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
             ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
             ((draw-pixmap main)"Tecla_blancasY.png"(make-posn 457.5 125))
             (define posicion_click_mouse (get-mouse-click main))
             (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 480)(< (posn-x(mouse-click-posn posicion_click_mouse)) 495)
                             (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                        (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 450)(< (posn-x(mouse-click-posn posicion_click_mouse)) 525)
                             (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                    [begin
                      (play-sound "LA1Tecla_blanca.wav" #t)                     ;Tecla blanca 6
                      ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
                      ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                      ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                      ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                      (define posicion_click_mouse (get-mouse-click main))
                      (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                      (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                 (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                      (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                             [begin
                               (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                               ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                               ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                               ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                               ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                               (define posicion_click_mouse (get-mouse-click main))
                               (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                               (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                          (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                               (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                      [begin
                                        (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                        ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                        ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                        ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                        ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                        (define posicion_click_mouse (get-mouse-click main))
                                        (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                        (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                   (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                        (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                               [begin
                                                 (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                 ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                 ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                 ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                 ((draw-pixmap main)"Tecla_blancasY.png"(make-posn 457.5 125))
                                                 (define posicion_click_mouse (get-mouse-click main))
                                                 (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 480)(< (posn-x(mouse-click-posn posicion_click_mouse)) 495)
                                                                 (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                            (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 450)(< (posn-x(mouse-click-posn posicion_click_mouse)) 525)
                                                                 (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                        [begin
                                                          (play-sound "LA1Tecla_blanca.wav" #t)                     ;Tecla blanca 6
                                                          ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
                                                          ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                          ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                          ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                          (define posicion_click_mouse (get-mouse-click main))
                                                          (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                          (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                     (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                          (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                 [begin
                                                                   (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                   ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                   ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                   ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                   ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                   (define posicion_click_mouse (get-mouse-click main))
                                                                   (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                   (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                              (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                   (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                          [begin
                                                                            (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                            ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                            ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                            ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                            ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                            (define posicion_click_mouse (get-mouse-click main))
                                                                            (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                            (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                       (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                            (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                   [begin
                                                                                     (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                     ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                     ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                     ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                     (define posicion_click_mouse (get-mouse-click main))
                                                                                     (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 645)
                                                                                                     (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 675)
                                                                                                     (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                            [begin
                                                                                              (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                              ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                              ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                              ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                              (define posicion_click_mouse (get-mouse-click main))
                                                                                              (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                     [begin
                                                                                                       (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                       ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                       ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                       ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                       (define posicion_click_mouse (get-mouse-click main))
                                                                                                       (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                       (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                  (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                       (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                              [begin
                                                                                                                (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                (define posicion_click_mouse (get-mouse-click main))
                                                                                                                (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 645)
                                                                                                                                (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                           (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 675)
                                                                                                                                (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                       [begin
                                                                                                                         (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                         ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                         ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                         ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                         ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                         (define posicion_click_mouse (get-mouse-click main))
                                                                                                                         (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                                                         (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                    (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                                                         (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                [begin
                                                                                                                                  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                  ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                  ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                  ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                  (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                  (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                                                                  (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                             (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                                                                  (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                         [begin
                                                                                                                                           (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                           ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                           ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                           ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                           ((draw-pixmap main)"Tecla_blancasP.png"(make-posn 757.5 125))
                                                                                                                                           (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                           (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 780)(< (posn-x(mouse-click-posn posicion_click_mouse)) 825)
                                                                                                                                                           (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                      (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 750)(< (posn-x(mouse-click-posn posicion_click_mouse)) 780)
                                                                                                                                                           (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                  [begin
                                                                                                                                                    (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
                                                                                                                                                    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 750 373))
                                                                                                                                                    ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                    ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                    ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                    (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                    (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                                                                                    (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                               (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                                                                                    (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                           [begin
                                                                                                                                                             (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                             ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                             ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                             ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                             ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                                                             (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                             (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 645)
                                                                                                                                                                             (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                        (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 675)
                                                                                                                                                                             (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                    [begin
                                                                                                                                                                      (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                                                                      ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                                                                      ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                      ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                      ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                                                                                      (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                      (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                                                                                      (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                 (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                                                                                      (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                             [begin
                                                                                                                                                                               (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                                                                               ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                                                                               ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                               ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                               ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                               (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                               (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                               (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                          (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                               (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                      [begin
                                                                                                                                                                                        (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                        ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                        ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                        ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                        ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                                                        (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                        (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                                                                                                                        (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                   (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                                                                                                                        (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                               [begin
                                                                                                                                                                                                 (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                                                                 ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                                                                 ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                 ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                 ((draw-pixmap main)"Tecla_blancasP.png"(make-posn 757.5 125))
                                                                                                                                                                                                 (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                 (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 780)(< (posn-x(mouse-click-posn posicion_click_mouse)) 825)
                                                                                                                                                                                                                 (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                            (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 750)(< (posn-x(mouse-click-posn posicion_click_mouse)) 780)
                                                                                                                                                                                                                 (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                        [begin
                                                                                                                                                                                                          (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
                                                                                                                                                                                                          ((draw-pixmap main)"Tecla_gris1.png"(make-posn 750 373))
                                                                                                                                                                                                          ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                          ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                          ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                                                                          (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                          (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
                                                                                                                                                                                                                          (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                     (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
                                                                                                                                                                                                                          (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                 [begin
                                                                                                                                                                                                                   (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                                                                                   ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                                                                                   ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                   ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                   ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                                                                                                                   (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                   (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 645)
                                                                                                                                                                                                                                   (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                              (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 675)
                                                                                                                                                                                                                                   (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))

                                                                                                                                                                                                                          [begin
                                                                                                                                                                                                                            (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                                                                                                                            ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                                                                                                                            ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                            ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                            ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                                                                                                                                            (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                            (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                                                                                                                                            (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                       (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
                                                                                                                                                                                                                                            (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                   [begin
                                                                                                                                                                                                                                     (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                                                                                                                                     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                                                                                                                                     ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                     ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                     ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                     (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                     (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                                                                                     (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                                                                                     (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                            [begin
                                                                                                                                                                                                                                              (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                              ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                              ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                              ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                              (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                              (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                                                                                              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                                                                                              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                     [begin
                                                                                                                                                                                                                                                       (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                       ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Tecla_blancasW.png"(make-posn 157.5 125))
                                                                                                                                                                                                                                                       (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                                       (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 180)(< (posn-x(mouse-click-posn posicion_click_mouse)) 195)
                                                                                                                                                                                                                                                                       (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                                  (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 150)(< (posn-x(mouse-click-posn posicion_click_mouse)) 225)
                                                                                                                                                                                                                                                                       (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                              [begin
                                                                                                                                                                                                                                                                (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                                                (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                                                                                                                (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                                           (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                                                                                                                (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                                       [begin
                                                                                                                                                                                                                                                                         (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                         ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                         (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                                                         (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                                                                                                                         (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                                                    (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                                                                                                                         (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                                                [begin
                                                                                                                                                                                                                                                                                  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                  ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Tecla_blancasW.png"(make-posn 157.5 125))
                                                                                                                                                                                                                                                                                  (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                                                                  (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 180)(< (posn-x(mouse-click-posn posicion_click_mouse)) 195)
                                                                                                                                                                                                                                                                                                  (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                                                             (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 150)(< (posn-x(mouse-click-posn posicion_click_mouse)) 225)
                                                                                                                                                                                                                                                                                                  (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                                                         [begin
                                                                                                                                                                                                                                                                                           (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                           ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                                           (define posicion_click_mouse (get-mouse-click main))
                                                                                                                                                                                                                                                                                           (cond ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
                                                                                                                                                                                                                                                                                                           (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
                                                                                                                                                                                                                                                                                                      (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
                                                                                                                                                                                                                                                                                                           (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))
                                                                                                                                                                                                                                                                                                  [begin
                                                                                                                                                                                                                                                                                                    (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                                    ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"winner.png"(make-posn 75 0))
                                                                                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                                                                                 (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                                           ])
                                                                                                                                                                                                                                                                                        (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                                                                               (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                         ])
                                                                                                                                                                                                                                                                      (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                                                             (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                       ])
                                                                                                                                                                                                                                                    (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                                           (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                     ])
                                                                                                                                                                                                                                  (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                         (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                   ])
                                                                                                                                                                                                                (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                          ])
                                                                                                                                                                                                       (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                 ])
                                                                                                                                                                                              (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                        ])
                                                                                                                                                                                     (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                               ])
                                                                                                                                                                            (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                      ])
                                                                                                                                                                   (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                             ])
                                                                                                                                                          (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                    ])
                                                                                                                                                 (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                           ])
                                                                                                                                        (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                  ])
                                                                                                                               (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                         ])
                                                                                                                      (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                ])
                                                                                                             (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                       ])
                                                                                                    (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                              ])
                                                                                           (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                     ])
                                                                                  (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                            ])
                                                                         (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                   ])
                                                                (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                          ])
                                                       (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                 ])
                                              (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                        ])
                                     (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                               ])
                            (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                      ])
                   (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
             ])
          (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
    ])

(define (oprimir_tecla main)
  (accion (key-value(get-key-press main)) main))
(define (accion tecla main)
  (if (equal? tecla 'release)
  (oprimir_tecla main)
  tecla))

(define (cancion_juego_1.1 main modo)
  [begin
    ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
    (define tecla (oprimir_tecla main))
    (cond ((equal? tecla #\t)
           [begin
             (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
             ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
             ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
             ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
             ((draw-pixmap main)"Tecla_blancasY.png"(make-posn 457.5 125))
             (define tecla (oprimir_tecla main))
             (cond ((equal? tecla #\y)
                    [begin
                      (play-sound "LA1Tecla_blanca.wav" #t)                     ;Tecla blanca 6
                      ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
                      ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                      ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                      ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                      (define tecla (oprimir_tecla main))
                      (cond ((equal? tecla #\u)
                             [begin
                               (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                               ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                               ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                               ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                               ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                               (define tecla (oprimir_tecla main))
                               (cond ((equal? tecla #\t)
                                      [begin
                                        (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                        ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                        ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                        ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                        ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                        (define tecla (oprimir_tecla main))
                                        (cond ((equal? tecla #\t)
                                               [begin
                                                 (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                 ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                 ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                 ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                 ((draw-pixmap main)"Tecla_blancasY.png"(make-posn 457.5 125))
                                                 (define tecla (oprimir_tecla main))
                                                 (cond ((equal? tecla #\y)
                                                        [begin
                                                          (play-sound "LA1Tecla_blanca.wav" #t)                     ;Tecla blanca 6
                                                          ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
                                                          ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                          ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                          ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                          (define tecla (oprimir_tecla main))
                                                          (cond ((equal? tecla #\u)
                                                                 [begin
                                                                   (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                   ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                   ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                   ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                   ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                   (define tecla (oprimir_tecla main))
                                                                   (cond ((equal? tecla #\t)
                                                                          [begin
                                                                            (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                            ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                            ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                            ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                            ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                            (define tecla (oprimir_tecla main))
                                                                            (cond ((equal? tecla #\u)
                                                                                   [begin
                                                                                     (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                     ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                     ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                     ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                     (define tecla (oprimir_tecla main))
                                                                                     (cond ((equal? tecla #\i)
                                                                                            [begin
                                                                                              (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                              ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                              ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                              ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                              (define tecla (oprimir_tecla main))
                                                                                              (cond ((equal? tecla #\o)
                                                                                                     [begin
                                                                                                       (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                       ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                       ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                       ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                       (define tecla (oprimir_tecla main))
                                                                                                       (cond ((equal? tecla #\u)
                                                                                                              [begin
                                                                                                                (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                (define tecla (oprimir_tecla main))
                                                                                                                (cond ((equal? tecla #\i)
                                                                                                                       [begin
                                                                                                                         (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                         ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                         ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                         ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                         ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                         (define tecla (oprimir_tecla main))
                                                                                                                         (cond ((equal? tecla #\o)
                                                                                                                                [begin
                                                                                                                                  (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                  ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                  ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                  ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                  (define tecla (oprimir_tecla main))
                                                                                                                                  (cond ((equal? tecla #\o)
                                                                                                                                         [begin
                                                                                                                                           (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                           ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                           ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                           ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                           ((draw-pixmap main)"Tecla_blancasP.png"(make-posn 757.5 125))
                                                                                                                                           (define tecla (oprimir_tecla main))
                                                                                                                                           (cond ((equal? tecla #\p)
                                                                                                                                                  [begin
                                                                                                                                                    (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
                                                                                                                                                    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 750 373))
                                                                                                                                                    ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                    ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                    ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                    (define tecla (oprimir_tecla main))
                                                                                                                                                    (cond ((equal? tecla #\o)
                                                                                                                                                           [begin
                                                                                                                                                             (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                             ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                             ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                             ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                             ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                                                             (define tecla (oprimir_tecla main))
                                                                                                                                                             (cond ((equal? tecla #\i)
                                                                                                                                                                    [begin
                                                                                                                                                                      (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                                                                      ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                                                                      ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                      ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                      ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                                                                                      (define tecla (oprimir_tecla main))
                                                                                                                                                                      (cond ((equal? tecla #\u)
                                                                                                                                                                             [begin
                                                                                                                                                                               (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                                                                               ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                                                                               ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                               ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                               ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                               (define tecla (oprimir_tecla main))
                                                                                                                                                                               (cond ((equal? tecla #\t)
                                                                                                                                                                                      [begin
                                                                                                                                                                                        (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                        ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                        ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                        ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                        ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                                                        (define tecla (oprimir_tecla main))
                                                                                                                                                                                        (cond ((equal? tecla #\o)
                                                                                                                                                                                               [begin
                                                                                                                                                                                                 (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                                                                 ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                                                                 ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                 ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                 ((draw-pixmap main)"Tecla_blancasP.png"(make-posn 757.5 125))
                                                                                                                                                                                                 (define tecla (oprimir_tecla main))
                                                                                                                                                                                                 (cond ((equal? tecla #\p)
                                                                                                                                                                                                        [begin
                                                                                                                                                                                                          (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
                                                                                                                                                                                                          ((draw-pixmap main)"Tecla_gris1.png"(make-posn 750 373))
                                                                                                                                                                                                          ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                          ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                          ((draw-pixmap main)"Tecla_blancasO.png"(make-posn 682.5 125))
                                                                                                                                                                                                          (define tecla (oprimir_tecla main))
                                                                                                                                                                                                          (cond ((equal? tecla #\o)
                                                                                                                                                                                                                 [begin
                                                                                                                                                                                                                   (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
                                                                                                                                                                                                                   ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
                                                                                                                                                                                                                   ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                   ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                   ((draw-pixmap main)"Tecla_blancasI.png"(make-posn 607.5 125))
                                                                                                                                                                                                                   (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                   (cond ((equal? tecla #\i)
                                                                                                                                                                                                                          [begin
                                                                                                                                                                                                                            (play-sound "DO2Tecla_blanca.wav" #t)                     ;Tecla blanca 8
                                                                                                                                                                                                                            ((draw-pixmap main)"Tecla_gris1.png"(make-posn 600 373))
                                                                                                                                                                                                                            ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                            ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                            ((draw-pixmap main)"Tecla_blancasU.png"(make-posn 532.5 125))
                                                                                                                                                                                                                            (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                            (cond ((equal? tecla #\u)
                                                                                                                                                                                                                                   [begin
                                                                                                                                                                                                                                     (play-sound "SI1Tecla_blanca.wav" #t)                     ;Tecla blanca 7
                                                                                                                                                                                                                                     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 525 373))
                                                                                                                                                                                                                                     ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                     ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                     ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                     (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                     (cond ((equal? tecla #\t)
                                                                                                                                                                                                                                            [begin
                                                                                                                                                                                                                                              (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                              ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                              ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                              ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                              (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                              (cond ((equal? tecla #\t)
                                                                                                                                                                                                                                                     [begin
                                                                                                                                                                                                                                                       (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                       ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                       ((draw-pixmap main)"Tecla_blancasW.png"(make-posn 157.5 125))
                                                                                                                                                                                                                                                       (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                                       (cond ((equal? tecla #\w)
                                                                                                                                                                                                                                                              [begin
                                                                                                                                                                                                                                                                (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                                                (cond ((equal? tecla #\t)
                                                                                                                                                                                                                                                                       [begin
                                                                                                                                                                                                                                                                         (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                         ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                         ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                         (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                                                         (cond ((equal? tecla #\t)
                                                                                                                                                                                                                                                                                [begin
                                                                                                                                                                                                                                                                                  (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                  ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                  ((draw-pixmap main)"Tecla_blancasW.png"(make-posn 157.5 125))
                                                                                                                                                                                                                                                                                  (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                                                                  (cond ((equal? tecla #\w)
                                                                                                                                                                                                                                                                                         [begin
                                                                                                                                                                                                                                                                                           (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                           ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                           ((draw-pixmap main)"Tecla_blancasT.png"(make-posn 382.5 125))
                                                                                                                                                                                                                                                                                           (define tecla (oprimir_tecla main))
                                                                                                                                                                                                                                                                                           (cond ((equal? tecla #\t)
                                                                                                                                                                                                                                                                                                  [begin
                                                                                                                                                                                                                                                                                                    (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
                                                                                                                                                                                                                                                                                                    ((draw-solid-rectangle main)(make-posn 75 0) 1050 250 (make-rgb 0.24 0.24 0.24))
                                                                                                                                                                                                                                                                                                    (sleep 1)
                                                                                                                                                                                                                                                                                                    ((draw-pixmap main)"winner.png"(make-posn 75 0))
                                                                                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                                                                                 (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                                           ])
                                                                                                                                                                                                                                                                                        (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                                                                               (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                         ])
                                                                                                                                                                                                                                                                      (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                                                             (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                                       ])
                                                                                                                                                                                                                                                    (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                                           (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                                     ])
                                                                                                                                                                                                                                  (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                         (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                                   ])
                                                                                                                                                                                                                (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                          ])
                                                                                                                                                                                                       (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                                 ])
                                                                                                                                                                                              (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                                        ])
                                                                                                                                                                                     (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                               ])
                                                                                                                                                                            (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                                      ])
                                                                                                                                                                   (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                             ])
                                                                                                                                                          (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                                    ])
                                                                                                                                                 (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                           ])
                                                                                                                                        (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                                  ])
                                                                                                                               (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                         ])
                                                                                                                      (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                                ])
                                                                                                             (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                                       ])
                                                                                                    (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                              ])
                                                                                           (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                                     ])
                                                                                  (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                            ])
                                                                         (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                                   ])
                                                                (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                          ])
                                                       (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                                 ])
                                              (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                                        ])
                                     (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                               ])
                            (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
                      ])
                   (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
             ])
          (else((draw-pixmap main)"gameover.jpg"(make-posn 75 0))))
    ])

#|Obtener por teclado|#

(define (teclado main teclas tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
(cond
  ((equal? teclas #\q)
   [begin
     (play-sound "DO1Tecla_blanca.wav" #t)                               ;Tecla blanca 1
     ((draw-pixmap main)"Tecla_gris.png"(make-posn 75 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\w)
   [begin
     (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\e)
   [begin
     (play-sound "MI1Tecla_blanca.wav" #t)                     ;Tecla blanca 3
     ((draw-pixmap main)"Tecla_gris2.png"(make-posn 225 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\r)
   [begin
     (play-sound "FA1Tecla_blanca.wav" #t)                               ;Tecla blanca 4
     ((draw-pixmap main)"Tecla_gris.png"(make-posn 300 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\t)
   [begin
     (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\y)
   [begin
     (play-sound "LA1Tecla_blanca.wav" #t)                         ;Tecla blanca 6
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\u)
   [begin
     (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
     ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\i)
   [begin
     (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
     ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\o)
   [begin
     (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main(key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\p)
   [begin
     (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
     ((draw-pixmap main)"Tecla_gris2.png"(make-posn 750 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ]) 
  ((equal? teclas #\f)
   [begin
     (play-sound "FA2Tecla_blanca.wav" #t)                               ;Tecla blanca 11
     ((draw-pixmap main)"Tecla_gris.png"(make-posn 825 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\g)
   [begin
     (play-sound "SOL2Tecla_blanca.wav" #t)                     ;Tecla blanca 12
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 900 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\h)
   [begin
     (play-sound "LA2Tecla_blanca.wav" #t)                         ;Tecla blanca 13
     ((draw-pixmap main)"Tecla_gris1.png"(make-posn 975 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\j)
   [begin
     (play-sound "SI2Tecla_blanca.wav" #t)                       ;Tecla blanca 14
     ((draw-pixmap main)"Tecla_gris2.png"(make-posn 1050 373))
     ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\1)
   [begin
     (play-sound "DO_1Tecla_negra.wav" #t)                          ;Tecla negra 1
     ((draw-solid-ellipse main)(make-posn 145 355)10 10 "red")
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 145 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\2)
   [begin
     (play-sound "RE1Tecla_negra.wav" #t)                             ;Tecla negra 2
     ((draw-solid-ellipse main)(make-posn 220 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 220 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\3)
   [begin
     (play-sound "FA_1Tecla_negra.wav" #t)                      ;Tecla negra 3
     ((draw-solid-ellipse main)(make-posn 370 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 370 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\4)
   [begin
     (play-sound "SOL1Tecla_negra.wav" #t)                          ;Tecla negra 4
     ((draw-solid-ellipse main)(make-posn 445 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 445 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\5)
   [begin
     (play-sound "LA_1Tecla_negra.wav" #t)                          ;Tecla negra 5
     ((draw-solid-ellipse main)(make-posn 520 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 520 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\6)
   [begin
     (play-sound "DO_2Tecla_negra.wav" #t)                          ;Tecla negra 6
     ((draw-solid-ellipse main)(make-posn 670 355)10 10 "red")
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 670 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\7)
   [begin
     (play-sound "RE2Tecla_negra.wav" #t)                             ;Tecla negra 7
     ((draw-solid-ellipse main)(make-posn 745 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 745 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\8)
   [begin
     (play-sound "FA_2Tecla_negra.wav" #t)                      ;Tecla negra 8
     ((draw-solid-ellipse main)(make-posn 895 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 895 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\9)
   [begin
     (play-sound "SOL2Tecla_negra.wav" #t)                          ;Tecla negra 9
     ((draw-solid-ellipse main)(make-posn 970 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 970 355) 10 10)
     (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\0)
   [begin
     (play-sound "LA_2Tecla_negra.wav" #t)                          ;Tecla negra 10
     ((draw-solid-ellipse main)(make-posn 1045 355)10 10 "red")    
     (sleep 0.1)
     ((clear-solid-ellipse main)(make-posn 1045 355) 10 10)
     (teclado main(key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\z)
   [begin
     (Dibujar_piano main(+ contador_posiciones 1) contador_click_canciones modo pantalla_canciones)
     ])
  ((equal? teclas #\x)
   (if (= contador_click_canciones 1)
       [begin
         (Dibujar_piano main contador_posiciones(+ 1 contador_click_canciones) modo "Pantalla_cancion_2.png")
         ]
       [begin
         (Dibujar_piano main contador_posiciones 1  modo "Pantalla_cancion_1.png")]
       ))
  ((equal? teclas #\c)
   (if (= contador_click_canciones 1)
       [begin
         (cancion1 main tipo_de_teclado)
         (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)]
       (if (= contador_click_canciones 2)
           [begin
             (cancion2 main tipo_de_teclado)
             (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)]
           (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
           )))
  ((equal? teclas #\v)
          [begin
            ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
            (cancion_juego_1.1 main modo)
            (sleep 5)
            (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)])
  (else (teclado main (key-value (get-key-press main))tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones))
  ))

#|Obtener click|#

(define (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
  (define click (get-mouse-click main))
  (piano_virtual main click tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones))

#|Piano virtual|#

(define (piano_virtual main posicion_click_mouse tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
  (cond
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 75)(< (posn-x(mouse-click-posn posicion_click_mouse)) 120)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 75)(< (posn-x(mouse-click-posn posicion_click_mouse)) 150)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "DO1Tecla_blanca.wav" #t)                               ;Tecla blanca 1
       ((draw-pixmap main)"Tecla_gris.png"(make-posn 75 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 120)(< (posn-x(mouse-click-posn posicion_click_mouse)) 180)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "DO_1Tecla_negra.wav" #t)                          ;Tecla negra 1
       ((draw-solid-ellipse main)(make-posn 145 355)10 10 "red")
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 145 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 180)(< (posn-x(mouse-click-posn posicion_click_mouse)) 195)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 150)(< (posn-x(mouse-click-posn posicion_click_mouse)) 225)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "RE1Tecla_blanca.wav" #t)                     ;Tecla blanca 2
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 150 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 195)(< (posn-x(mouse-click-posn posicion_click_mouse)) 255)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "RE1Tecla_negra.wav" #t)                             ;Tecla negra 2
       ((draw-solid-ellipse main)(make-posn 220 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 220 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 255)(< (posn-x(mouse-click-posn posicion_click_mouse)) 300)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 225)(< (posn-x(mouse-click-posn posicion_click_mouse)) 255)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "MI1Tecla_blanca.wav" #t)                     ;Tecla blanca 3
       ((draw-pixmap main)"Tecla_gris2.png"(make-posn 225 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 300)(< (posn-x(mouse-click-posn posicion_click_mouse)) 345)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 345)(< (posn-x(mouse-click-posn posicion_click_mouse)) 375)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "FA1Tecla_blanca.wav" #t)                               ;Tecla blanca 4
       ((draw-pixmap main)"Tecla_gris.png"(make-posn 300 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 345)(< (posn-x(mouse-click-posn posicion_click_mouse)) 405)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "FA_1Tecla_negra.wav" #t)                      ;Tecla negra 3
       ((draw-solid-ellipse main)(make-posn 370 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 370 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 420)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 375)(< (posn-x(mouse-click-posn posicion_click_mouse)) 450)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SOL1Tecla_blanca.wav" #t)                     ;Tecla blanca 5
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 375 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 405)(< (posn-x(mouse-click-posn posicion_click_mouse)) 480)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SOL1Tecla_negra.wav" #t)                          ;Tecla negra 4
       ((draw-solid-ellipse main)(make-posn 445 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 445 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 480)(< (posn-x(mouse-click-posn posicion_click_mouse)) 495)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 450)(< (posn-x(mouse-click-posn posicion_click_mouse)) 525)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "LA1Tecla_blanca.wav" #t)                         ;Tecla blanca 6
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 450 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 495)(< (posn-x(mouse-click-posn posicion_click_mouse)) 555)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "LA_1Tecla_negra.wav" #t)                          ;Tecla negra 5
       ((draw-solid-ellipse main)(make-posn 520 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 520 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 555)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 525)(< (posn-x(mouse-click-posn posicion_click_mouse)) 600)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SI1Tecla_blanca.wav" #t)                       ;Tecla blanca 7
       ((draw-pixmap main)"Tecla_gris2.png"(make-posn 525 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 645)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 600)(< (posn-x(mouse-click-posn posicion_click_mouse)) 675)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "DO2Tecla_blanca.wav" #t)                               ;Tecla blanca 8
       ((draw-pixmap main)"Tecla_gris.png"(make-posn 600 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 645)(< (posn-x(mouse-click-posn posicion_click_mouse)) 705)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "DO_2Tecla_negra.wav" #t)                          ;Tecla negra 6
       ((draw-solid-ellipse main)(make-posn 670 355)10 10 "red")
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 670 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 705)(< (posn-x(mouse-click-posn posicion_click_mouse)) 720)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 675)(< (posn-x(mouse-click-posn posicion_click_mouse)) 750)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "RE2Tecla_blanca.wav" #t)                     ;Tecla blanca 9
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 675 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 720)(< (posn-x(mouse-click-posn posicion_click_mouse)) 780)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "RE2Tecla_negra.wav" #t)                             ;Tecla negra 7
       ((draw-solid-ellipse main)(make-posn 745 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 745 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 780)(< (posn-x(mouse-click-posn posicion_click_mouse)) 825)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 750)(< (posn-x(mouse-click-posn posicion_click_mouse)) 780)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "MI2Tecla_blanca.wav" #t)                     ;Tecla blanca 10
       ((draw-pixmap main)"Tecla_gris2.png"(make-posn 750 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 825)(< (posn-x(mouse-click-posn posicion_click_mouse)) 870)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 870)(< (posn-x(mouse-click-posn posicion_click_mouse)) 900)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "FA2Tecla_blanca.wav" #t)                               ;Tecla blanca 11
       ((draw-pixmap main)"Tecla_gris.png"(make-posn 825 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 870)(< (posn-x(mouse-click-posn posicion_click_mouse)) 930)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "FA_2Tecla_negra.wav" #t)                      ;Tecla negra 8
       ((draw-solid-ellipse main)(make-posn 895 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 895 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 930)(< (posn-x(mouse-click-posn posicion_click_mouse)) 945)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 900)(< (posn-x(mouse-click-posn posicion_click_mouse)) 975)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SOL2Tecla_blanca.wav" #t)                     ;Tecla blanca 12
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 900 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 930)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1005)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SOL2Tecla_negra.wav" #t)                          ;Tecla negra 9
       ((draw-solid-ellipse main)(make-posn 970 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 970 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 1005)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1020)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 975)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1050)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "LA2Tecla_blanca.wav" #t)                         ;Tecla blanca 13
       ((draw-pixmap main)"Tecla_gris1.png"(make-posn 975 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 1020)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1080)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 509)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "LA_2Tecla_negra.wav" #t)                          ;Tecla negra 10
       ((draw-solid-ellipse main)(make-posn 1045 355)10 10 "red")    
       (sleep 0.1)
       ((clear-solid-ellipse main)(make-posn 1045 355) 10 10)
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((or (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 1080)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1125)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 373)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574))
         (and (> (posn-x(mouse-click-posn posicion_click_mouse)) 1050)(< (posn-x(mouse-click-posn posicion_click_mouse)) 1125)
              (> (posn-y(mouse-click-posn posicion_click_mouse)) 509)(< (posn-y(mouse-click-posn posicion_click_mouse)) 574)))        ;Evaluamos la posicion del clik en X y Y
     [begin
       (play-sound "SI2Tecla_blanca.wav" #t)                       ;Tecla blanca 14
       ((draw-pixmap main)"Tecla_gris2.png"(make-posn 1050 373))
       ((draw-pixmap main)tipo_de_teclado(make-posn 74 373))
       (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
       ])
    ((and (> (posn-x(mouse-click-posn posicion_click_mouse)) 152.5)(< (posn-x(mouse-click-posn posicion_click_mouse)) 232.5)
          (> (posn-y(mouse-click-posn posicion_click_mouse)) 270)(< (posn-y(mouse-click-posn posicion_click_mouse)) 330))
     [begin
       (Dibujar_piano main(+ contador_posiciones 1) contador_click_canciones modo pantalla_canciones)]
     )
    ((and (> (posn-x(mouse-click-posn posicion_click_mouse)) 330)(< (posn-x(mouse-click-posn posicion_click_mouse)) 478)
          (> (posn-y(mouse-click-posn posicion_click_mouse)) 260)(< (posn-y(mouse-click-posn posicion_click_mouse)) 338))
     (if (= contador_click_canciones 1)
         [begin
           (Dibujar_piano main contador_posiciones(+ 1 contador_click_canciones) modo "Pantalla_cancion_2.png")
           ]
         [begin
           (Dibujar_piano main contador_posiciones 1  modo "Pantalla_cancion_1.png")]
         ))
    ((and (> (posn-x(mouse-click-posn posicion_click_mouse)) 720)(< (posn-x(mouse-click-posn posicion_click_mouse)) 800)
          (> (posn-y(mouse-click-posn posicion_click_mouse)) 270)(< (posn-y(mouse-click-posn posicion_click_mouse)) 330))
     (if (= contador_click_canciones 1)
         [begin
           (cancion1 main tipo_de_teclado)
           (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)]
         (if (= contador_click_canciones 2)
             [begin
               (cancion2 main tipo_de_teclado)
               (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)]
             (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones)
             )))
    ((and (> (posn-x(mouse-click-posn posicion_click_mouse)) 820)(< (posn-x(mouse-click-posn posicion_click_mouse)) 900)
          (> (posn-y(mouse-click-posn posicion_click_mouse)) 270)(< (posn-y(mouse-click-posn posicion_click_mouse)) 330))
     (cond ((= modo 1)
            [begin
              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
              (cancion_juego_1 main modo)
              (sleep 5)
              (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)])
           (else
            [begin
              ((draw-pixmap main)"Imagen_letras_y_numeros.jpeg"(make-posn 74 373))
              (cancion_juego_1.1 main modo)
              (sleep 5)
              (Dibujar_piano main contador_posiciones contador_click_canciones modo pantalla_canciones)]
            ))
     )
    (else (obtenerclick main tipo_de_teclado contador_posiciones contador_click_canciones modo pantalla_canciones))
    ))

(pregunta)