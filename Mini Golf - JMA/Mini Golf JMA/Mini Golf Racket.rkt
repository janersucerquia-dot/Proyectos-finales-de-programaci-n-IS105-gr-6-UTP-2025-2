;; --- IMPORTACIONES ---
(require 2htdp/image)
(require 2htdp/universe)
(require racket/gui/base)
(require racket/runtime-path)

;; --- CONSTANTES DE PANTALLA ---
(define WIDTH 1408)
(define HEIGHT 736)




;; --- CONSTANTES DE FÍSICA ---
(define BALL-RADIO 15)
(define HOLE-RADIO 18)
(define FRICCION 0.98)
(define FUERZA 0.08) 
(define MAX-TIROS 10) ;; NUEVO: Límite de tiros por nivel

;; --- IMÁGENES GLOBALES ---
(define IMAGEN-BOLA (circle BALL-RADIO "solid" "white"))
(define IMAGEN-HOLE (circle HOLE-RADIO "solid" "transparent"))
(define IMAGEN-CANDADO (overlay (text "🔒" 40 "black") (square 80 "solid" "gray")))
(define FONDO (bitmap/file "menu.png"))
(define MENU01 (bitmap/file "selector-nivel01.png"))
(define MENU02 (bitmap/file "selector-nivel02.png"))
(define MENU03 (bitmap/file "selector-nivel03.png"))
(define LEVEL-CCOMPLETED( bitmap/file "nivel-completado.png"))
(define game-over (bitmap/file "game-over.png"))
(define nivel01 (bitmap/file "nivel-01.png"))
(define nivel02 (bitmap/file "nivel-02.png"))
(define nivel03 (bitmap/file "nivel-03.png"))


(define-runtime-path musica-fondo "soundtrack.wav")

(define (iniciar-musica)
  (thread
   (lambda()
     (let loop()
       (if(file-exists? musica-fondo)
          (play-sound musica-fondo #f)
          (void))
       (loop)))))


;; --- CONSTANTES DE INTERFAZ (BOTONES) ---
;; Botón Continuar (Escena 3 - Victoria)
(define BTN-CONT-X 535)
(define BTN-CONT-Y 580)
(define BTN-CONT-W 300)
(define BTN-CONT-H 120)
(define IMG-BTN-CONT (overlay (text "Continuar" 20 "white") (rectangle BTN-CONT-W BTN-CONT-H "solid" "green")))

;; Botón Reintentar (Escena 3 y 4)
(define BTN-RETRY-X 885)
(define BTN-RETRY02-X 600)
(define BTN-RETRY-Y 580)
(define IMG-BTN-RETRY (overlay (text "Reintentar" 20 "black") (rectangle BTN-CONT-W BTN-CONT-H "solid" "yellow")))

;; Botón Salir (Escena 1 - Selector)
(define BTN-EXIT-X 695)
(define BTN-EXIT-Y 645)
(define IMG-BTN-EXIT (overlay (text "MENÚ" 15 "white") (rectangle 300 100 "solid" "red")))


;; --- CONFIGURACIÓN DE NIVELES ---
(define (obtener-inicio-nivel n)
  (cond [(= n 1) (make-posn 100 300)]        
        [(= n 2) (make-posn 790 570)]        
        [(= n 3) (make-posn 1240 590)]     
        [else (make-posn 100 300)]))

(define (obtener-hoyo-nivel n)
  (cond [(= n 1) (make-posn 1255 585)]   
        [(= n 2) (make-posn 152 155)]     
        [(= n 3) (make-posn 1240 170)]             
        [else (make-posn (- WIDTH 100) 300)]))

;; --- ESTRUCTURA DEL JUEGO ---
;; NUEVO: Agregamos el campo 'tiros'
;; escena: 0=Menu, 1=Selector, 2=Jugando, 3=Victoria, 4=Game Over
(define-struct estado (escena nivel-actual desbloqueados tiros bola mouse-ini mouse-act))
(define-struct bola (x y vx vy))

;; Estado Inicial: tiros comienza en MAX-TIROS
(define ESTADO-INICIAL 
  (make-estado 0 1 1 MAX-TIROS (make-bola 0 0 0 0) #false #false))

;; --- HITBOXES ---
(define (clic-en-caja? mx my x y w h)
  (and (> mx (- x (/ w 2))) (< mx (+ x (/ w 2)))
       (> my (- y (/ h 2))) (< my (+ y (/ h 2)))))

;HITBOXES NIVEL 1
(define circulo-nivel01(circle 118 "solid" "brown"))

(define tronco01-nivel01(rectangle 100 41 "solid" "brown"))

(define tronco02-nivel01(rectangle 102 41 "solid" "brown"))

(define arbusto01-nivel01(rectangle 39 48 "solid" "green"))

(define arbusto02-nivel01(rectangle 58 46 "solid" "green"))

(define arbusto03-nivel01(rectangle 60 56  "solid" "green"))

(define arbusto04-nivel01(rectangle 50 47 "solid" "green"))

(define arbusto05-nivel01(rectangle 49 47 "solid" "green"))

(define arbusto06-nivel01(rectangle 49 55 "solid" "green"))

(define arbusto07-nivel01(rectangle 50 55 "solid" "green"))

(define piedra01-nivel01(rectangle 40 43 "solid" "gray"))

(define piedra02-nivel01(rectangle 36 36 "solid" "gray"))

(define piedra03-nivel01(rectangle 32 29 "solid" "gray"))

(define cerca01-nivel01(rectangle 22 310 "solid" "brown"))

(define cerca02-nivel01(rectangle 303 6 "solid" "brown"))

(define cerca03-nivel01(rectangle 27 197 "solid" "brown"))

(define cerca04-nivel01(rectangle 303 20  "solid" "brown"))

(define cerca05-nivel01(rectangle 305 25 "solid" "brown"))

(define cerca06-nivel01(rectangle 30 310 "solid" "brown"))

(define cerca07-nivel01(rectangle 30 215 "solid" "brown"))

(define cerca08-nivel01(rectangle 305 20 "solid" "brown"))


;; --- FÍSICA ---
(define (mover-bola b nivel) ;; <--- OJO: Ahora necesitamos saber el nivel
  (let* ((sig-x (+ (bola-x b) (bola-vx b)))
         (sig-y (+ (bola-y b) (bola-vy b)))
         (vel-x (* (bola-vx b) FRICCION))
         (vel-y (* (bola-vy b) FRICCION)))
    
    (make-bola
     ;; 1. Mover X (Si choca pared O choca obstáculo, rebota)
     (cond 
       [(or (>= (+ sig-x BALL-RADIO) WIDTH) 
            (<= (- sig-x BALL-RADIO) 0)
            (choca-con-obstaculo? sig-x (bola-y b) nivel)) ;; <-- NUEVO: Check Obstáculo Horizontal
        (cond ;; Si choca, retrocede un poco para no quedar pegado
           [(> (bola-vx b) 0) (- sig-x 5)]
           [else (+ sig-x 5)])]
       [else sig-x])

     ;; 2. Mover Y
     (cond 
       [(or (>= (+ sig-y BALL-RADIO) HEIGHT) 
            (<= (- sig-y BALL-RADIO) 0)
            (choca-con-obstaculo? (bola-x b) sig-y nivel)) ;; <-- NUEVO: Check Obstáculo Vertical
        (cond
           [(> (bola-vy b) 0) (- sig-y 5)]
           [else (+ sig-y 5)])]
       [else sig-y])

     ;; 3. Velocidad X (Rebote)
     (cond 
       [(or (>= (+ sig-x BALL-RADIO) WIDTH) (<= (- sig-x BALL-RADIO) 0)
            (choca-con-obstaculo? sig-x (bola-y b) nivel)) ;; <-- Si choca, invierte
        (* vel-x -0.8)]
       [else vel-x])

     ;; 4. Velocidad Y (Rebote)
     (cond 
       [(or (>= (+ sig-y BALL-RADIO) HEIGHT) (<= (- sig-y BALL-RADIO) 0)
            (choca-con-obstaculo? (bola-x b) sig-y nivel)) ;; <-- Si choca, invierte
        (* vel-y -0.8)]
       [else vel-y]))))

(define (distancia x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

;; --- HERRAMIENTAS MATEMÁTICAS PARA COLISIONES ---

;; 1. Detecta si la bola choca con un rectángulo
;; (bx, by) = bola, (rx, ry) = centro obstáculo, (w, h) = tamaño obstáculo


(define (chocar-rectangulo? bx by rx ry w h)
  (let ([zona-segura-w (+ (/ w 2) BALL-RADIO)]  ;; Ancho del muro + Radio bola
        [zona-segura-h (+ (/ h 2) BALL-RADIO)]) ;; Alto del muro + Radio bola
    
    (and (> bx (- rx zona-segura-w))  ;; Más a la derecha que el borde izquierdo "expandido"
         (< bx (+ rx zona-segura-w))  ;; Más a la izquierda que el borde derecho "expandido"
         (> by (- ry zona-segura-h))  ;; Más abajo que el techo "expandido"
         (< by (+ ry zona-segura-h))))) ;; Más arriba que el piso "expandido"

;; 2. Detecta si la bola choca con un círculo
(define (chocar-circulo? bx by cx cy radio-obstaculo)
  (< (distancia bx by cx cy) (+ BALL-RADIO radio-obstaculo)))

;; --- FUNCIÓN PRINCIPAL DE COLISIONES ---
;; Esta es la que estaba incompleta. Aquí puse tus coordenadas.
(define (choca-con-obstaculo? x y nivel)
  (cond
    ;; --- OBSTÁCULOS NIVEL 1 ---
    [(= nivel 1)
     (or 
      ;; Troncos
      (chocar-rectangulo? x y 705 95 100 40)
      
      (chocar-rectangulo? x y 705 655 100 40)
      ;; Círculo
      (chocar-circulo? x y 705 360 118)
      ;; Piedras
      (chocar-circulo? x y 1207 115 20)
      
      (chocar-circulo? x y 185 620 30)
      ;; Arbustos
      (chocar-circulo? x y 450 66 24)
      
      (chocar-circulo? x y 92 96 24)
      
      (chocar-circulo? x y 532 70 24)
      
      (chocar-circulo? x y 1316 94 30)
      
      (chocar-circulo? x y 88 655 25 )
      
      (chocar-circulo? x y 880 670 30)
      
      (chocar-circulo? x y 990 670 30)
      
      ;; Cercas
    (chocar-rectangulo? x y 196 52 328 24)
    
      (chocar-rectangulo? x y  43 193 15 305)
      
      (chocar-rectangulo? x y 207 691 306 22)
      
      (chocar-rectangulo? x y  43 596  22 212)
      
      (chocar-rectangulo? x y 1213 691 330 22)
      
      (chocar-rectangulo? x y 1360 585 30 190 )
      
      (chocar-rectangulo? x y 1213 51 330 22)
      
      (chocar-rectangulo? x y 1360 203 20 285 )
      )]

    ;; --- OBSTÁCULOS NIVEL 2 (Vacío por ahora) ---
    [(= nivel 2)
     (or
      (chocar-rectangulo? x y 710 332 120 60)
      
      (chocar-circulo? x y 653 472 59)
      
      (chocar-rectangulo? x y 695 85 100 40)
      
      (chocar-circulo? x y 535 70 30)
      
      (chocar-circulo? x y 412 65 25)
      
      (chocar-circulo? x y 995 670 30)
      
      (chocar-circulo? x y 880 670 30)
      
      (chocar-circulo? x y 90 655 25)
      
      (chocar-circulo? x y 185 620 25)
      
      (chocar-circulo? x y 107 580 28)
      
      (chocar-circulo? x y 545 450 20)
      
      (chocar-circulo? x y 475 420 20)
      
      (chocar-circulo? x y 600 560 50)
      
      (chocar-circulo? x y 390 580 60)

      ;huecos
      
      (chocar-rectangulo? x y 155 220 105 22)
      
      (chocar-rectangulo? x y 155 75 105 22)
      
      (chocar-rectangulo? x y 583 190 675 10)
      
      (chocar-rectangulo? x y 583 136 675 10)

      ;cercas
      
      (chocar-rectangulo? x y 196 52 328 24)
      
      (chocar-rectangulo? x y  43 193 15 305)
      
      (chocar-rectangulo? x y 207 691 306 22)
      
      (chocar-rectangulo? x y  43 596  22 212)
      
      (chocar-rectangulo? x y 1213 691 330 22)
      
      (chocar-rectangulo? x y 1360 585 30 190 )
      
      (chocar-rectangulo? x y 1213 51 330 22)
      
      (chocar-rectangulo? x y 1360 203 20 285 )
      )]     
    ;; --- OBSTÁCULOS NIVEL 3 (Vacío por ahora) ---
    [(= nivel 3)
     (or
      ;(chocar-circulo? x y 1230 560 115)
      (chocar-rectangulo? x y 615 627 1050 10)
      
      (chocar-rectangulo? x y 635 577 960 10)
      
      (chocar-rectangulo? x y  145 362 10 420)
      
      (chocar-rectangulo? x y  95 365 10 522)
      
      (chocar-rectangulo? x y 453 103 905 10)
      
      (chocar-rectangulo? x y 569 151 835 10)
      
      (chocar-rectangulo? x y 196 52 328 24)
      
      (chocar-rectangulo? x y  43 193 15 305)
      
      (chocar-rectangulo? x y 207 691 306 22)
      
      (chocar-rectangulo? x y  43 596  22 212)
      
      (chocar-rectangulo? x y 1213 691 330 22)
      
      (chocar-rectangulo? x y 1360 585 30 190 )
      
      (chocar-rectangulo? x y 1213 51 330 22)
      
      (chocar-rectangulo? x y 1360 203 20 285 )
      )
      ]

    [else #false]))

;; Auxiliar: ¿La bola está casi quieta? (Velocidad menor a 0.1)
(define (bola-detenida? b)
  (and (< (abs (bola-vx b)) 0.1) (< (abs (bola-vy b)) 0.1)))



;; --- ACTUALIZAR MUNDO ---
(define (actualizar e)
  (cond
    ;; Solo calculamos física si estamos en ESCENA 2 (Jugando)
    [(not (= (estado-escena e) 2)) e]
    
    [else
     (let* ([b (estado-bola e)]
            [nivel (estado-nivel-actual e)]
            [hoyo (obtener-hoyo-nivel nivel)]
            [dist (distancia (bola-x b) (bola-y b) (posn-x hoyo) (posn-y hoyo))])
       
       (cond
         ;; 1. VICTORIA: Entró al hoyo
         [(< dist 15)
          (make-estado 3 nivel (estado-desbloqueados e) (estado-tiros e) b #false #false)]
         
         ;; 2. GAME OVER: Se acabaron los tiros Y la bola se detuvo
         [(and (= (estado-tiros e) 0) (bola-detenida? b))
          (make-estado 4 nivel (estado-desbloqueados e) 0 b #false #false)] ;; Escena 4 = Perdiste
         
         ;; 3. SEGUIR JUGANDO
         [else
          (make-estado 2 nivel (estado-desbloqueados e) (estado-tiros e)
                       (mover-bola b nivel) (estado-mouse-ini e) (estado-mouse-act e))]))]))

;; --- MANEJADOR DE MOUSE ---
(define (mouse-handler e x y event)
  (cond
    ;; --- ESCENA 0: MENÚ ---
    [(= (estado-escena e) 0)
     (if (and (string=? event "button-down")
              (clic-en-caja? x y (/ WIDTH 1.95)590 345 130)) 
         (make-estado 1 1 (estado-desbloqueados e) MAX-TIROS (estado-bola e) #false #false)
         e)]

    ;; --- ESCENA 1: SELECTOR DE NIVELES ---
    [(= (estado-escena e) 1)
     (if (string=? event "button-down")
         (cond
           ;; Botón Salir al Menú
           [(clic-en-caja? x y BTN-EXIT-X BTN-EXIT-Y 300 100)
            (make-estado 0 1 (estado-desbloqueados e) MAX-TIROS (make-bola 0 0 0 0) #false #false)] 
           
           ;; Selección de Niveles (Reinicia los tiros a MAX-TIROS)
           [(clic-en-caja? x y 250 345 415 320) (iniciar-nivel e 1)]
           [(and (clic-en-caja? x y 705 345 415 320) (>= (estado-desbloqueados e) 2)) (iniciar-nivel e 2)]
           [(and (clic-en-caja? x y 1170 345 415 320) (>= (estado-desbloqueados e) 3)) (iniciar-nivel e 3)]
           [else e])
         e)]

    ;; --- ESCENA 2: JUGANDO ---
    [(= (estado-escena e) 2)
     ;; PROTECCIÓN: Solo permitimos disparar si hay tiros > 0 Y la bola está quieta
     (if (and (> (estado-tiros e) 0) (bola-detenida? (estado-bola e)))
         (cond
           [(string=? event "button-down")
            (make-estado 2 (estado-nivel-actual e) (estado-desbloqueados e) (estado-tiros e) (estado-bola e) (make-posn x y) (make-posn x y))]
       
           [(string=? event "drag")
            (if (posn? (estado-mouse-ini e))
                (make-estado 2 (estado-nivel-actual e) (estado-desbloqueados e) (estado-tiros e) (estado-bola e) (estado-mouse-ini e) (make-posn x y))
                e)]
       
           [(string=? event "button-up")
            (if (posn? (estado-mouse-ini e))
                (let* ([ini (estado-mouse-ini e)]
                       [dx (- (posn-x ini) x)]
                       [dy (- (posn-y ini) y)])
                  ;; AQUÍ RESTAMOS EL TIRO: (- (estado-tiros e) 1)
                  (make-estado 2 (estado-nivel-actual e) (estado-desbloqueados e) 
                               (- (estado-tiros e) 1) 
                               (make-bola (bola-x (estado-bola e)) (bola-y (estado-bola e)) (* dx FUERZA) (* dy FUERZA))
                               #false #false))
                e)]
           [else e])
         e)] ;; Si no cumple la protección, ignoramos el evento

    ;; --- ESCENA 3 (VICTORIA) y ESCENA 4 (GAME OVER) ---
    [(or (= (estado-escena e) 3) (= (estado-escena e) 4))
     (if (string=? event "button-down")
         (cond
           ;; Botón CONTINUAR (Solo funciona si ganaste, escena 3)
           [(and (= (estado-escena e) 3) (clic-en-caja? x y BTN-CONT-X BTN-CONT-Y BTN-CONT-W BTN-CONT-H))
            (let ([nivel (estado-nivel-actual e)])
              (make-estado 1 
                           nivel 
                           (if (< nivel 3) (max (estado-desbloqueados e) (+ nivel 1)) 3) 
                           MAX-TIROS (make-bola 0 0 0 0) #false #false))]
           
           ;; Botón REINTENTAR (Funciona en Victoria y en Game Over)
           [(or(clic-en-caja? x y BTN-RETRY-X BTN-RETRY-Y BTN-CONT-W BTN-CONT-H)
               (clic-en-caja? x y BTN-RETRY02-X BTN-RETRY-Y BTN-CONT-W BTN-CONT-H))
            (iniciar-nivel e (estado-nivel-actual e))]
           
           [else e])
         e)]
    
    [else e]))

(define (iniciar-nivel e n)
  (let ([inicio (obtener-inicio-nivel n)])
    ;; Al iniciar nivel, TIROS vuelve a MAX-TIROS (5)
    (make-estado 2 n (estado-desbloqueados e) MAX-TIROS 
                 (make-bola (posn-x inicio) (posn-y inicio) 0 0) 
                 #false #false)))

;; --- DIBUJADO ---
(define (dibujar e)
  (cond
    ;; --- MENU ---
    [(= (estado-escena e) 0)
     FONDO]
;     (place-image (overlay (text "JUGAR" 30 "white") (rectangle 345 130 "solid" "blue"))
;                  (/ WIDTH 1.95) 590                  
;                               FONDO)]

    ;; --- SELECTOR ---
    [(= (estado-escena e) 1)
          (cond
            ((=(estado-desbloqueados e)1)MENU01)
            ((= (estado-desbloqueados e)2)MENU02)
            (else
             MENU03))]
          ;MENU02
          ;MENU03     ]
;     (place-image IMG-BTN-EXIT BTN-EXIT-X BTN-EXIT-Y 
;      (place-image (text "SELECCIONA UN NIVEL" 40 "black") (/ WIDTH 2) 100
;       (place-image (dibujar-boton-nivel 1 (estado-desbloqueados e)) 200 300
;        (place-image (dibujar-boton-nivel 2 (estado-desbloqueados e)) 400 300
;         (place-image (dibujar-boton-nivel 3 (estado-desbloqueados e)) 600 300
;          (rectangle WIDTH HEIGHT "solid" "lightblue"))))))]

    ;; --- JUEGO (ESCENA 2) ---
    [(= (estado-escena e) 2) (dibujar-juego-base e)]
    
    ;; --- VICTORIA (ESCENA 3) ---
    [(= (estado-escena e) 3)
     LEVEL-CCOMPLETED]
    
    ;; --- GAME OVER (ESCENA 4) ---
    [(= (estado-escena e) 4)
     game-over])) 

;; Función auxiliar para dibujar el juego y el HUD
(define (dibujar-juego-base e)
  (let* ([b (estado-bola e)]
         [hoyo (obtener-hoyo-nivel (estado-nivel-actual e))]
         [m1 (estado-mouse-ini e)]
         [m2 (estado-mouse-act e)]
         (fondo-actual (cond
                         ((=(estado-nivel-actual e)1)nivel01)
                         ((=(estado-nivel-actual e)2)nivel02)
                         (else
                          nivel03))))
    
    ;; DIBUJAMOS EL HUD (Información en pantalla)
    (place-image (text (string-append "NIVEL " (number->string (estado-nivel-actual e))) 20 "black") 50 30
     (place-image (text (string-append "TIROS: " (number->string (estado-tiros e))) 20 "red") (- WIDTH 80) 30 ;; <-- CONTADOR DE TIROS
      (place-image IMAGEN-HOLE (posn-x hoyo) (posn-y hoyo)
       (place-image IMAGEN-BOLA (bola-x b) (bola-y b)
        (if (and (posn? m1) (posn? m2))
            (add-line (add-line fondo-actual  (posn-x m1) (posn-y m1) (posn-x m2) (posn-y m2) "gray")
                      (posn-x m1) (posn-y m1) (bola-x b) (bola-y b) "black")
            fondo-actual )))))))

(define (dibujar-boton-nivel n desbloqueados)
  (if (>= desbloqueados n)
      (overlay (text (number->string n) 40 "black") (square 80 "solid" "green"))
      MENU01))

(define (teclas e k)
  (if (key=? k "r") (iniciar-nivel e (estado-nivel-actual e)) e))


(iniciar-musica)

(big-bang ESTADO-INICIAL
  (on-tick actualizar)
  (on-mouse mouse-handler)
  (on-key teclas)
  (to-draw dibujar))
