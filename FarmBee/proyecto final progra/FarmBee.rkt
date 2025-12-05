(require (lib "graphics.ss" "graphics"))
(require racket/gui)
(require racket/runtime-path)
(open-graphics)
(define ventana (open-viewport "Farm Bee" 1180 800))
(define buffer (open-pixmap "Buffer" 1180 800))

(define-runtime-path sonido-juego "popcorn.wav")

(define (sonar-musica)
  (thread
   (lambda ()
     (let loop ()
       (play-sound sonido-juego #f)
       (sleep 0.1)                  
       (loop)))))

; memoria de flores, 8 espacios
(define flores-colocadas (make-vector 8 #f))

; portada
((draw-pixmap ventana) "fondo.jpg" (make-posn 0 0))
((draw-solid-ellipse ventana) (make-posn 440 600) 300 60 "gold")
((draw-ellipse ventana) (make-posn 440 600) 300 60 "black")
((draw-string ventana) (make-posn 570 630) "PLAY")
(sonar-musica)

(define clic (get-mouse-click ventana))
(define pos (mouse-click-posn clic))

(define (click-en-play? x y)
  (and (>= x 440) (<= x 740)
       (>= y 600) (<= y 660)))

; dibujar abejitaasss (son las vidas en el juego)
(define (abeja-mini x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 30 18 "yellow")
  ((draw-solid-rectangle buffer)(make-posn (+ x 8) (+ y 3)) 6 12 "black")
  ((draw-solid-rectangle buffer)(make-posn (+ x 18) (+ y 3)) 6 12 "black")
  ((draw-solid-ellipse buffer)(make-posn (+ x 5) (- y 8)) 15 10 "lightblue")
  ((draw-solid-ellipse buffer)(make-posn (+ x 15) (- y 8)) 15 10 "lightblue"))

(define (dibujar-vidas vidas)
  (cond
    [(= vidas 3)
     (abeja-mini 1000 20)
     (abeja-mini 1045 20)
     (abeja-mini 1090 20)]
    [(= vidas 2)
     (abeja-mini 1000 20)
     (abeja-mini 1045 20)]
    [(= vidas 1)
     (abeja-mini 1000 20)]
    [else 'sin-vidas]))

; dibujar aREPA
(define (dibujar-abeja x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 60 40 "yellow")
  ((draw-solid-rectangle buffer)(make-posn (+ x 10) (+ y 5)) 10 30 "black")
  ((draw-solid-rectangle buffer)(make-posn (+ x 30) (+ y 5)) 10 30 "black")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) (- y 15)) 30 20 "lightblue")
  ((draw-solid-ellipse buffer)(make-posn (+ x 5) (- y 15)) 30 20 "lightblue"))

; dibujar mosquita (enemigooo)
(define (dibujar-mosquita x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 60 40 "black")
  ((draw-solid-ellipse buffer)(make-posn x (- y 8)) 30 15 "lightblue")
  ((draw-solid-ellipse buffer)(make-posn (+ x 35) (- y 8)) 30 15 "lightblue"))

; florecitaaas (4 florecitas)
(define (flor-rosa x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "yellow")
  ((draw-solid-rectangle buffer)(make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-margarita x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "brown")
  ((draw-solid-rectangle buffer)(make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-morada x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "pink")
  ((draw-solid-rectangle buffer)(make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-roja x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "black")
  ((draw-solid-rectangle buffer)(make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (dibujar-flor num x y)
  (cond
    [(= num 0) (flor-rosa x y)]
    [(= num 1) (flor-margarita x y)]
    [(= num 2) (flor-morada x y)]
    [(= num 3) (flor-roja x y)]))

; dibujar semillitas
(define (dibujar-semilla x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 30 50 "chocolate")
  ((draw-ellipse buffer)(make-posn x y) 30 50 "brown"))

(define (dibujar-todas-semillas)
  (dibujar-semilla 200 400)
  (dibujar-semilla 300 400)
  (dibujar-semilla 400 400)
  (dibujar-semilla 500 400)
  (dibujar-semilla 600 400)
  (dibujar-semilla 700 400)
  (dibujar-semilla 800 400)
  (dibujar-semilla 900 400))

; detectar si toco o toca (es una colision)
(define (toca-semilla? abeja-x abeja-y semilla-x semilla-y)
  (and (< (abs (- abeja-x semilla-x)) 40)
       (< (abs (- abeja-y semilla-y)) 60)))

(define (toca-mosquita? abeja-x abeja-y mosquita-x mosquita-y)
  (and (< (abs (- abeja-x mosquita-x)) 50)
       (< (abs (- abeja-y mosquita-y)) 35)))

; plantar una flor (con memoria), es como lo de vectores punto 1 parcial 3
(define (plantar-flor abeja-x abeja-y semilla-x semilla-y indice)
  (cond
    [(vector-ref flores-colocadas indice)
     (dibujar-flor (vector-ref flores-colocadas indice) semilla-x semilla-y)]
    [(toca-semilla? abeja-x abeja-y semilla-x semilla-y)
     (dibujar-flor (random 4) semilla-x semilla-y)
     (vector-set! flores-colocadas indice (random 4))]
    [else 'nada]))

;  revisar las 8 semillas
(define (revisar-8-semillas abeja-x abeja-y)
  (plantar-flor abeja-x abeja-y 200 400 0)
  (plantar-flor abeja-x abeja-y 300 400 1)
  (plantar-flor abeja-x abeja-y 400 400 2)
  (plantar-flor abeja-x abeja-y 500 400 3)
  (plantar-flor abeja-x abeja-y 600 400 4)
  (plantar-flor abeja-x abeja-y 700 400 5)
  (plantar-flor abeja-x abeja-y 800 400 6)
  (plantar-flor abeja-x abeja-y 900 400 7))

;calcula la direccion donde se mueven las mosquitas
(define (calcular-nueva-y y-actual direccion)
  (cond
    [(<= (+ y-actual direccion) 200) 200]; que llegue a 200
    [(>= (+ y-actual direccion) 760) 760]; que llegue a 760
    [else (+ y-actual direccion)]));moverse

; solo calcula la nueva dirección hacia arriba o hacia abajo
(define (calcular-nueva-direccion y-actual direccion)
  (cond
    [(<= (+ y-actual direccion) 200) 20]; toca arriba (200) baja
    [(>= (+ y-actual direccion) 760) -20]; toca abajo (760) sube
    [else direccion]));mantiene la direccion hacia arriba o hacia abajo
;el 30 y -30 ayuda a cambiar de velocidad

; la pantalla roja que se muestra cuando pierde una vida
(define (mostrar-golpe)
  ((draw-solid-rectangle buffer)(make-posn 0 0) 1180 800 "red")
  (copy-viewport buffer ventana)
  (sleep 0.4))

; GAME OVER
(define (pantalla-game-over)
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "red")
  ((draw-string ventana)(make-posn 500 400) "GAME OVER")
  (sleep 3))

; obtener posicion del mouse
; en x
(define (obtener-x-mouse)
  (posn-x (query-mouse-posn ventana)))
; en y
(define (obtener-y-mouse)
  (posn-y (query-mouse-posn ventana)))

; boton go sirve para pasar de nivel
(define (boton-go x y)
  ((draw-solid-rectangle buffer)(make-posn x y) 100 50 "green")
  ((draw-rectangle buffer)(make-posn x y) 100 50 "black")
  ((draw-string buffer)(make-posn (+ x 40) (+ y 30)) "GO"))

; ¿La abeja toca el botón GO?
; boton x - donde empieza el boton
; boton y - donde empieza el boton
(define (toca-boton-go? abeja-x abeja-y boton-x boton-y)
  (and (>= abeja-x boton-x)
       (<= abeja-x (+ boton-x 100))
       (>= abeja-y boton-y)
       (<= abeja-y (+ boton-y 50))))

;   LOOP PRINCIPAL - con recursion
;   vidas = cuántas vidas tiene (3, 2, 1), las abejitaash 
;   m1y = posición Y de mosquita 1 (izquierda)
;   m1dir = dirección de mosquita 1 
;   m2y = posición Y de mosquita 2 (derecha)
;   m2dir = dirección de mosquita 2

(define (jugar vidas m1y m1dir m2y m2dir)
  ; limpia y dibuja fondo
  ((draw-solid-rectangle buffer)(make-posn 0 0) 1180 800 "white")
  ((draw-pixmap buffer) "level.jpg" (make-posn 0 0))
  ; dibuja vidas (abejitash)
  (dibujar-vidas vidas)
  ; dibujar semillitas
  (dibujar-todas-semillas)
  ; dibujar botón GO 
  (boton-go 1050 700)
  ; revisar si toca una semilla pra plantar flor (usando posicion del mouse)
  (revisar-8-semillas (obtener-x-mouse) (obtener-y-mouse))
  ; dibuja mosquitas
  (dibujar-mosquita 200 m1y)
  (dibujar-mosquita 900 m2y)
  ; dibuja abeja ( con en el mouse)
  (dibujar-abeja (obtener-x-mouse) (obtener-y-mouse))
  ; mostrar en pantalla lo que puse en el buffer
  (copy-viewport buffer ventana)
  (sleep 0.0001)
  ; toca botones o mosquitas?
  (cond
    ; si toca boton go, pasa al siguiente nivel
    [(toca-boton-go? (obtener-x-mouse) (obtener-y-mouse) 1050 700)
     ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "gold")
     ((draw-string ventana)(make-posn 500 400) "NIVEL 2!")
     (sleep 2)
     (nivel-2)] ; llama los otros nivelsitos
    ; si toca mosquita enemiga x100pre pierde vida
    [(or (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) 200 m1y)
         (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) 900 m2y))
     (mostrar-golpe)
     (cond
       [(<= (- vidas 1) 0) (pantalla-game-over)]              ; Game over (perdiste mi papacho)
       [else (jugar (- vidas 1) 600 -20 300 20)])]            ; puedes seguir jugando
    ; si no ha tocao nadita ps sigue jugando (AQUÍ se mueven las mosquitas)
    [else
     (jugar vidas 
            (calcular-nueva-y m1y m1dir)                     
            (calcular-nueva-direccion m1y m1dir)             
            (calcular-nueva-y m2y m2dir)                     
            (calcular-nueva-direccion m2y m2dir))]))          


; empieza el nivel 1
(define (nivel-1)
  ((draw-pixmap ventana) "level.jpg" (make-posn 0 0))
  (dibujar-vidas 3)
  (dibujar-todas-semillas)
  (sleep 0.2)
  ; Llamar a jugar con valores iniciales:
  ; 3 vidas
  ; Mosquita 1: Y=600, dirección=-25 (subiendo)
  ; Mosquita 2: Y=300, dirección=25 (bajando)
  (jugar 3 600 -25 300 25))

;   NIVEL 2
(define (dibujar-semillas-nivel-2)
  (dibujar-semilla 250 250)  
  (dibujar-semilla 450 300)   
  (dibujar-semilla 650 250)  
  (dibujar-semilla 850 300)   
  (dibujar-semilla 200 550)   
  (dibujar-semilla 400 600)   
  (dibujar-semilla 700 600)   
  (dibujar-semilla 900 550)) 

(define (revisar-8-semillas-nivel-2 abeja-x abeja-y)
  (plantar-flor abeja-x abeja-y 250 250 0)
  (plantar-flor abeja-x abeja-y 450 300 1)
  (plantar-flor abeja-x abeja-y 650 250 2)
  (plantar-flor abeja-x abeja-y 850 300 3)
  (plantar-flor abeja-x abeja-y 200 550 4)
  (plantar-flor abeja-x abeja-y 400 600 5)
  (plantar-flor abeja-x abeja-y 700 600 6)
  (plantar-flor abeja-x abeja-y 900 550 7))

; Loop del nivel 2 con 3 mosquitas
(define (jugar-nivel-2 vidas m1y m1dir m2y m2dir m3y m3dir)
  ((draw-solid-rectangle buffer)(make-posn 0 0) 1180 800 "white")
  ((draw-pixmap buffer) "level.jpg" (make-posn 0 0))
  (dibujar-vidas vidas)
  (dibujar-semillas-nivel-2)
  (boton-go 540 100)
  (revisar-8-semillas-nivel-2 (obtener-x-mouse) (obtener-y-mouse))
  ; dibuja 3 mosquitas
  (dibujar-mosquita 150 m1y)
  (dibujar-mosquita 550 m2y)   
  (dibujar-mosquita 950 m3y)   
  ; dibuja abeja
  (dibujar-abeja (obtener-x-mouse) (obtener-y-mouse))
  (copy-viewport buffer ventana)
  (sleep 0.0001)
  ; toca botones o mosquitas?
  (cond
    ; si toca boton go, pasa al siguiente nivel
    [(toca-boton-go? (obtener-x-mouse) (obtener-y-mouse) 540 100)
     ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "gold")
     ((draw-string ventana)(make-posn 400 380) "¡FELICIDADES, entraste al nivel 3!")
     (sleep 3)
     (nivel-3)]
    ; si toca cualquiera de las 3 mosquitas pierde vida
    [(or (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) 150 m1y)
         (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) 550 m2y)
         (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) 950 m3y))
     (mostrar-golpe)
     (cond
       [(<= (- vidas 1) 0) (pantalla-game-over)]
       [else (jugar-nivel-2 (- vidas 1) 600 -25 400 35 300 -25)])]
    ; si no ha tocao nadita sigue jugando
    [else
     (jugar-nivel-2 vidas 
                    (calcular-nueva-y m1y m1dir)
                    (calcular-nueva-direccion m1y m1dir)
                    (calcular-nueva-y m2y m2dir)
                    (calcular-nueva-direccion m2y m2dir)
                    (calcular-nueva-y m3y m3dir)
                    (calcular-nueva-direccion m3y m3dir))]))

(define (nivel-2)
  ; reiniciar o ps como cambiar la memoria de flores
  (vector-set! flores-colocadas 0 #f)
  (vector-set! flores-colocadas 1 #f)
  (vector-set! flores-colocadas 2 #f)
  (vector-set! flores-colocadas 3 #f)
  (vector-set! flores-colocadas 4 #f)
  (vector-set! flores-colocadas 5 #f)
  (vector-set! flores-colocadas 6 #f)
  (vector-set! flores-colocadas 7 #f)
  
  ; mensajito
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "pink")
  ((draw-string ventana)(make-posn 450 330) "NIVEL 2")
  ((draw-string ventana)(make-posn 320 380) "¡Ahora hay 3 mosquitas!")
  ((draw-string ventana)(make-posn 300 420) "¡Y se mueven más rápido!")
  ((draw-string ventana)(make-posn 250 460) "¡Las semillas están dispersas!")
  (sleep 5)
  
  ((draw-pixmap ventana) "level.jpg" (make-posn 0 0))
  (dibujar-vidas 3)
  (dibujar-semillas-nivel-2)
  (sleep 0.2)
  
  ; NIVEL 2: 3 mosquitas más rápidas
  (jugar-nivel-2 3 600 -25 400 35 300 -25))

;   NIVEL 3 
; Dibujar semillas en forma de S de Sofia (la que hizo el jueguito) jajaja
(define (dibujar-semillas-nivel-3)
  (dibujar-semilla 350 250)   
  (dibujar-semilla 500 220)   
  (dibujar-semilla 650 250)  
  (dibujar-semilla 550 350)   
  (dibujar-semilla 450 450)  
  (dibujar-semilla 350 550)   
  (dibujar-semilla 500 580)   
  (dibujar-semilla 650 550))

; Revisar las 8 semillas del NIVEL 3
(define (revisar-8-semillas-nivel-3 abeja-x abeja-y)
  (plantar-flor abeja-x abeja-y 350 250 0)
  (plantar-flor abeja-x abeja-y 500 220 1)
  (plantar-flor abeja-x abeja-y 650 250 2)
  (plantar-flor abeja-x abeja-y 550 350 3)
  (plantar-flor abeja-x abeja-y 450 450 4)
  (plantar-flor abeja-x abeja-y 350 550 5)
  (plantar-flor abeja-x abeja-y 500 580 6)
  (plantar-flor abeja-x abeja-y 650 550 7))

; Calcular nueva X (movimiento tipo Pacman)
(define (nueva-x-pacman x vx)
  (cond
    [(<= (+ x vx) 100) 100]
    [(>= (+ x vx) 1080) 1080]
    [else (+ x vx)]))
; Calcular nueva Y (movimiento tipo Pacman)
(define (nueva-y-pacman y vy)
  (cond
    [(<= (+ y vy) 200) 200]
    [(>= (+ y vy) 760) 760]
    [else (+ y vy)]))

; Cambiar velocidad X
(define (nueva-velocidad-x vx x)
  (cond
    [(<= x 100) 25]      
    [(>= x 1080) -25]    
    [else vx]))          

; Cambiar velocidad Y 
(define (nueva-velocidad-y vy y)
  (cond
    [(<= y 200) 25]    
    [(>= y 760) -25]     
    [else vy]))         

; Loop del nivel 3 
(define (jugar-nivel-3 vidas m1x m1y vx1 vy1 m2x m2y vx2 vy2 m3x m3y vx3 vy3)
  ((draw-solid-rectangle buffer)(make-posn 0 0) 1180 800 "white")
  ((draw-pixmap buffer) "level.jpg" (make-posn 0 0))
  (dibujar-vidas vidas)
  (dibujar-semillas-nivel-3)
  (boton-go 540 720)
  (revisar-8-semillas-nivel-3 (obtener-x-mouse) (obtener-y-mouse))
  (dibujar-mosquita m1x m1y)
  (dibujar-mosquita m2x m2y)
  (dibujar-mosquita m3x m3y)
  (dibujar-abeja (obtener-x-mouse) (obtener-y-mouse))
  (copy-viewport buffer ventana)
  (sleep 0.001)
  
  (cond
    [(toca-boton-go? (obtener-x-mouse) (obtener-y-mouse) 540 720)
     ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "gold")
     ((draw-pixmap ventana) "abejita-tierna.jpg" (make-posn 0 0))
     (sleep 5)
     (close-viewport ventana)]
    
    [(or (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) m1x m1y)
         (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) m2x m2y)
         (toca-mosquita? (obtener-x-mouse) (obtener-y-mouse) m3x m3y))
     (mostrar-golpe)
     (cond
       [(<= (- vidas 1) 0) (pantalla-game-over)]
       [else (jugar-nivel-3 (- vidas 1) 
                            (+ 200 (random 700)) (+ 250 (random 400)) 25 -24
                            (+ 200 (random 700)) (+ 250 (random 400)) -26 25
                            (+ 200 (random 700)) (+ 250 (random 400)) -25 -26)])]
    
    [else
     (jugar-nivel-3 vidas 
                    (nueva-x-pacman m1x vx1)
                    (nueva-y-pacman m1y vy1)
                    (nueva-velocidad-x vx1 m1x)
                    (nueva-velocidad-y vy1 m1y)
                    (nueva-x-pacman m2x vx2)
                    (nueva-y-pacman m2y vy2)
                    (nueva-velocidad-x vx2 m2x)
                    (nueva-velocidad-y vy2 m2y)
                    (nueva-x-pacman m3x vx3)
                    (nueva-y-pacman m3y vy3)
                    (nueva-velocidad-x vx3 m3x)
                    (nueva-velocidad-y vy3 m3y))]))


;   FUNCIÓN NIVEL-3
(define (nivel-3)
  (vector-set! flores-colocadas 0 #f)
  (vector-set! flores-colocadas 1 #f)
  (vector-set! flores-colocadas 2 #f)
  (vector-set! flores-colocadas 3 #f)
  (vector-set! flores-colocadas 4 #f)
  (vector-set! flores-colocadas 5 #f)
  (vector-set! flores-colocadas 6 #f)
  (vector-set! flores-colocadas 7 #f)
  
  ((draw-solid-rectangle ventana)(make-posn 0 0) 1180 800 "violet")
  ((draw-string ventana)(make-posn 400 300) "NIVEL 3 FINAL")
  ((draw-string ventana)(make-posn 250 350) "Las mosquitas se mueven ALEATORIAMENTE!")
  ((draw-string ventana)(make-posn 280 400) "Planta las flores en forma de S!")
  ((draw-string ventana)(make-posn 320 450) "Este es el desafio final JEJEJEJE!")
  (sleep 5)
  
  ((draw-pixmap ventana) "level.jpg" (make-posn 0 0))
  (dibujar-vidas 3)
  (dibujar-semillas-nivel-3)
  (sleep 0.3)
  
  ; Velocidades iniciales 
  (jugar-nivel-3 3 
                 300 400 25 -24
                 600 300 -26 25
                 900 500 -25 -26))

(if (click-en-play? (posn-x pos) (posn-y pos))  
    (nivel-1)                                    
    (displayln "clic fuera del boton"))          