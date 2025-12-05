#lang racket
(require 2htdp/image)
(require 2htdp/universe   (only-in racket/gui play-sound)
         racket/runtime-path)
(require lang/posn)
(define-runtime-path sonido-juego
  "musica.mp3")

 (define (sonar-musica)
  (thread
   (lambda ()
     (let loop ()
       (play-sound sonido-juego #f)
       (sleep 0.1)                  
       (loop)))))
(define ancho-ventana 1520)
(define alto-ventana 775)
(define grosor-pared 40)

;;;;;;;;;;


;; =========================
;; FONDO CON IMAGEN
;; =========================
(define fondo-img-original
  (bitmap "C:\\Users\\EDWUIN C\\OneDrive\\Pictures\\Pared de fondo2.png"))

(define fondo-img
  (scale/xy
   (/ ancho-ventana (image-width fondo-img-original))
   (/ alto-ventana  (image-height fondo-img-original))
   fondo-img-original))

;; =========================
;; PERSONAJE
;; =========================
(define personaje (bitmap "C:\\Users\\EDWUIN C\\OneDrive\\Pictures\\fireboy.png"))
(define personaje-img (scale (/ 50 (image-height personaje)) personaje))

;; 🧱 BLOQUES DESLIZABLES
(define bloque-img   (rectangle 60 60 "solid" "gray"))
(define bloque-lado  60)
(define bloque-radio (/ bloque-lado 2))

;; posiciones *mutables* de las cajas
(define bloque1-x 530)
(define bloque1-y 525)
(define bloque2-x 1110)
(define bloque2-y 435)

(struct estado (pos vel) #:transparent)
(struct vel (vx vy) #:transparent)

;; tamaño del mono (centro, radios)
(define pj-altura (image-height personaje-img))
(define pj-ancho  (image-width  personaje-img))
(define pj-radio-y (/ pj-altura 2))
(define pj-radio-x (/ pj-ancho  2))

;; posición inicial
(define estado-inicial (estado (make-posn 50 680) (vel 0 0)))

(define gravedad 1)
(define friccion 0.8)

;; =========================
;; FONDO
;; =========================
(define (dibujar-fondo)
  (place-image
   (rectangle ancho-ventana grosor-pared "solid" (make-color 10 22 10)) ; techo
   (/ ancho-ventana 2) 20
   (place-image
    (rectangle ancho-ventana grosor-pared "solid" (make-color 10 22 10)) ; suelo visual
    (/ ancho-ventana 2) (- alto-ventana 20)
    (place-image
     (rectangle grosor-pared alto-ventana "solid" (make-color 10 22 10)) ; pared izquierda
     20 (/ alto-ventana 2)
     (place-image
      (rectangle grosor-pared alto-ventana "solid" (make-color 10 22 10)) ; pared derecha
      (- ancho-ventana 20) (/ alto-ventana 2)
      (place-image fondo-img
                   (/ ancho-ventana 2)
                   (/ alto-ventana 2)
                   (empty-scene ancho-ventana alto-ventana)))))))

;; =========================
;; PLATAFORMAS DIBUJADAS
;; =========================
(define (dibujar-plataformas escena-base)
  (place-image (rectangle 1160 30 "solid" (make-color 111 125 80)) 900 150
  (place-image (polygon (list (make-posn 20 0)(make-posn 140 0)(make-posn 180 20)(make-posn -20 20))
                        "solid" (make-color 111 125 80)) 1100 125
  (place-image (rectangle 200 40 "solid" (make-color 111 125 80)) 420 115
  (place-image (rectangle 1240 30 "solid" (make-color 111 125 80)) 660 320
  (place-image (rectangle 120 85 "solid" (make-color 111 125 80)) 100 265
  (place-image (rectangle 180 80 "solid" (make-color 111 125 80)) 340 200
  (place-image (rectangle 250 80 "solid" (make-color 111 125 80)) 685 270
  (place-image (polygon (list (make-posn 0 0)(make-posn 71 0)(make-posn 71 50)(make-posn 35 0))
                        "solid" (make-color 111 125 80)) 945 360
  (place-image (rectangle 300 50 "solid" (make-color 111 125 80)) 1130 360
  (place-image (rectangle 600 30 "solid" (make-color 111 125 80)) 1180 480
  (place-image (polygon (list (make-posn 0 0)(make-posn 70 0)(make-posn 120 70)(make-posn 50 70))
                        "solid" (make-color 111 125 80)) 845 460
  (place-image (rectangle 570 30 "solid" (make-color 111 125 80)) 525 440
  (place-image (rectangle 1220 30 "solid" (make-color 111 125 80)) 650 570
  (place-image (rectangle 1440 30 "solid" (make-color 111 125 80)) 760 720
  (place-image (rectangle 170 70 "solid" (make-color 111 125 80)) 1395 670
               escena-base))))))))))))))))

;; =========================
;; LÍNEAS DE COLISIÓN (SOLO MAPA FIJO)
;; =========================
(struct linea (x1 y1 x2 y2) #:transparent)

(define lineas-colision
  (list
   (linea 40 705 1310 705)   
   (linea 1310 705 1310 635)
   (linea 1310 635 1480 635)
   (linea 40 585 1260 585)
   (linea 1260 555 1260 585)
   (linea 1260 555 40 555)
   (linea 240 455 805 455)
   (linea 240 455 240 425)
   (linea 240 425 805 425)
   (linea 805 425 855 425)
   (linea 855 425 885 465)   ; diagonales: las ignoramos en colisión
   (linea 905 495 835 495)
   (linea 905 495 835 495)
   (linea 905 495 1480 495)
   (linea 885 465 1480 465)
   (linea 835 495 805 455)
   (linea 1280 385 980 385)
   (linea 980 385 945 335)
   (linea 40 335 945 335)
   (linea 1280 385 1280 305)
   (linea 1280 305 810 305)
   (linea 810 231 810 305)
   (linea 810 231 560 231)
   (linea 560 305 560 231)
   (linea 560 305 160 305)
   (linea 160 305 160 223)
   (linea 40 223 160 223)
   (linea 250 240 430 240)
   (linea 430 165 430 240)
   (linea 430 165 1480 165)
   (linea 250 240 250 160)
   (linea 250 160 320 160)
   (linea 320 95 320 160)
   (linea 320 95 520 95)
   (linea 520 135 520 95)
   (linea 520 135 1000 135)
   (linea 1000 135 1040 115)
   (linea 1160 115 1040 115)
   (linea 1160 115 1200 135)
   (linea 1200 135 1480 135)))

(define (dibujar-lineas escena-base)
  (foldl
   (lambda (ln esc)
     (add-line esc
               (linea-x1 ln) (linea-y1 ln)
               (linea-x2 ln) (linea-y2 ln)
               "white"))
   escena-base
   lineas-colision))

;; =========================
;; UTILIDADES DE COLISIÓN
;; =========================
(define (linea-horizontal? ln)
  (= (linea-y1 ln) (linea-y2 ln)))

(define (linea-vertical? ln)
  (= (linea-x1 ln) (linea-x2 ln)))

;; ---------- Caída sobre plataformas y cajas ----------
(define (pisar-plataformas x0 y0 y1 vy1)
  (if (<= vy1 0)
      (values y1 vy1 #f)
      (let* ([pie0 (+ y0 pj-radio-y)]
             [pie1 (+ y1 pj-radio-y)]
             [izq0 (- x0 pj-radio-x)]
             [der0 (+ x0 pj-radio-x)])
        ;; plataformas del mapa
        (define (scan-lineas ls mejor-y)
          (if (null? ls)
              mejor-y
              (let* ([ln (first ls)]
                     [yL (linea-y1 ln)]
                     [x1 (linea-x1 ln)]
                     [x2 (linea-x2 ln)]
                     [xmin (min x1 x2)]
                     [xmax (max x1 x2)])
                (if (and (linea-horizontal? ln)
                         (<= pie0 yL) (>= pie1 yL)
                         (<= xmin der0) (>= xmax izq0))
                    (let ([nuevo-mejor (if (or (not mejor-y)
                                               (< yL mejor-y))
                                           yL
                                           mejor-y)])
                      (scan-lineas (cdr ls) nuevo-mejor))
                    (scan-lineas (cdr ls) mejor-y)))))
        ;; tapas de las cajas
        (define (check-bloque bx by mejor)
          (let* ([top (- by bloque-radio)]
                 [bleft (- bx bloque-radio)]
                 [bright (+ bx bloque-radio)])
            (if (and (<= pie0 top) (>= pie1 top)
                     (<= bleft der0) (>= bright izq0))
                (if (or (not mejor) (< top mejor)) top mejor)
                mejor)))
        (define mejor-y
          (check-bloque bloque2-x bloque2-y
            (check-bloque bloque1-x bloque1-y
                          (scan-lineas lineas-colision #f))))
        (if mejor-y
            (values (- mejor-y pj-radio-y) 0 #t)
            (values y1 vy1 #f)))))

;; ---------- Golpear techos ----------
(define (golpear-techo x0 y0 y1 vy1)
  (if (>= vy1 0)
      (values y1 vy1 #f)
      (let* ([cabeza0 (- y0 pj-radio-y)]
             [cabeza1 (- y1 pj-radio-y)]
             [izq0 (- x0 pj-radio-x)]
             [der0 (+ x0 pj-radio-x)])
        (let loop ([ls lineas-colision]
                   [mejor-y #f])
          (if (null? ls)
              (if mejor-y
                  (values (+ mejor-y pj-radio-y) 0 #t)
                  (values y1 vy1 #f))
              (let* ([ln (first ls)]
                     [yL (linea-y1 ln)]
                     [x1 (linea-x1 ln)]
                     [x2 (linea-x2 ln)]
                     [xmin (min x1 x2)]
                     [xmax (max x1 x2)])
                (if (and (linea-horizontal? ln)
                         (>= cabeza0 yL) (<= cabeza1 yL)
                         (<= xmin der0) (>= xmax izq0))
                    (let ([nuevo-mejor (if (or (not mejor-y)
                                               (> yL mejor-y))
                                           yL
                                           mejor-y)])
                      (loop (cdr ls) nuevo-mejor))
                    (loop (cdr ls) mejor-y))))))))

;; ---------- Paredes verticales del mapa ----------
(define (bloquear-paredes x0 y x1 vx1)
  (let* ([top (- y pj-radio-y)]
         [bottom (+ y pj-radio-y)]
         [izq0 (- x0 pj-radio-x)]
         [der0 (+ x0 pj-radio-x)]
         [izq1 (- x1 pj-radio-x)]
         [der1 (+ x1 pj-radio-x)])
    (let loop ([ls lineas-colision]
               [mejor-x x1]
               [mejor-vx vx1])
      (if (null? ls)
          (values mejor-x mejor-vx)
          (let* ([ln (first ls)]
                 [xL (linea-x1 ln)]
                 [y1 (linea-y1 ln)]
                 [y2 (linea-y2 ln)]
                 [ymin (min y1 y2)]
                 [ymax (max y1 y2)])
            (cond
              [(and (linea-vertical? ln)
                    (<= ymin bottom) (>= ymax top))
               (cond
                 [(and (> vx1 0)
                       (<= der0 xL) (>= der1 xL))
                  (loop (cdr ls) (- xL pj-radio-x) 0)]
                 [(and (< vx1 0)
                       (>= izq0 xL) (<= izq1 xL))
                  (loop (cdr ls) (+ xL pj-radio-x) 0)]
                 [else
                  (loop (cdr ls) mejor-x mejor-vx)])]
              [else
               (loop (cdr ls) mejor-x mejor-vx)]))))))

;; ---------- ¿Está sobre suelo (mapa o cajas)? ----------
(define (sobre-algun-suelo? x y)
  (let* ([pie (+ y pj-radio-y)]
         [izq (- x pj-radio-x)]
         [der (+ x pj-radio-x)])
    (define (sobre-bloque? bx by)
      (let* ([top (- by bloque-radio)]
             [bleft (- bx bloque-radio)]
             [bright (+ bx bloque-radio)])
        (and (<= bleft der) (>= bright izq)
             (<= (abs (- pie top)) 3))))
    (or
     (<= (abs (- pie 705)) 2)
     (for/or ([ln lineas-colision])
       (and (linea-horizontal? ln)
            (let* ([yL (linea-y1 ln)]
                   [x1 (linea-x1 ln)]
                   [x2 (linea-x2 ln)]
                   [xmin (min x1 x2)]
                   [xmax (max x1 x2)])
              (and (<= xmin der) (>= xmax izq)
                   (<= (abs (- pie yL)) 3)))))
     (sobre-bloque? bloque1-x bloque1-y)
     (sobre-bloque? bloque2-x bloque2-y))))

;; ---------- Empujar cajas izquierda / derecha ----------
(define (empujar-bloques x0 y x1 vx)
  (define dx (- x1 x0)) ; cuánto se quiere mover el mono en X

  (define (empujar-uno x0 x1 y vx bx by)
    (define right0 (+ x0 pj-radio-x))
    (define right1 (+ x1 pj-radio-x))
    (define left0  (- x0 pj-radio-x))
    (define left1  (- x1 pj-radio-x))
    (define top1   (- y pj-radio-y))
    (define bottom1 (+ y pj-radio-y))

    (define bleft  (- bx bloque-radio))
    (define bright (+ bx bloque-radio))
    (define btop   (- by bloque-radio))
    (define bbottom (+ by bloque-radio))

    (cond
      ;; empujar desde la izquierda hacia la derecha
      [(and (> vx 0)
            (<= right0 bleft)
            (>  right1 bleft)
            (< top1 bbottom)
            (> bottom1 btop))
       (let* ([nuevo-bx (+ bx dx)]
              ;; limitar dentro de la ventana
              [nuevo-bx (min (- ancho-ventana bloque-radio 20)
                             (max (+ bloque-radio 20) nuevo-bx))]
              [nuevo-bleft (- nuevo-bx bloque-radio)]
              [nuevo-x (- nuevo-bleft pj-radio-x)])
         (values nuevo-x nuevo-bx by))]

      ;; empujar desde la derecha hacia la izquierda
      [(and (< vx 0)
            (>= left0 bright)
            (<  left1 bright)
            (< top1 bbottom)
            (> bottom1 btop))
       (let* ([nuevo-bx (+ bx dx)]
              [nuevo-bx (min (- ancho-ventana bloque-radio 20)
                             (max (+ bloque-radio 20) nuevo-bx))]
              [nuevo-bright (+ nuevo-bx bloque-radio)]
              [nuevo-x (+ nuevo-bright pj-radio-x)])
         (values nuevo-x nuevo-bx by))]

      [else
       (values x1 bx by)]))

  ;; primero caja 1, luego caja 2
  (let-values ([(x-after bx1 by1)
                (empujar-uno x0 x1 y vx bloque1-x bloque1-y)])
    (set! bloque1-x bx1)
    (set! bloque1-y by1)
    (let-values ([(x-final bx2 by2)
                  (empujar-uno x0 x-after y vx bloque2-x bloque2-y)])
      (set! bloque2-x bx2)
      (set! bloque2-y by2)
      x-final)))

;; =========================
;; DIBUJAR MUNDO
;; =========================
(define (dibujar-bloques escena-base)
  (place-image bloque-img bloque1-x bloque1-y
   (place-image bloque-img bloque2-x bloque2-y
                escena-base)))

(define (dibujar mundo)
  (place-image personaje-img
               (posn-x (estado-pos mundo))
               (posn-y (estado-pos mundo))
               (dibujar-lineas
                (dibujar-bloques
                 (dibujar-plataformas
                  (dibujar-fondo))))))

;; =========================
;; ACTUALIZAR FÍSICA
;; =========================
(define (actualizar mundo)
  (let* ([pos        (estado-pos mundo)]
         [vel-actual (estado-vel mundo)]
         [x0   (posn-x pos)]
         [y0   (posn-y pos)]
         [vx0  (vel-vx vel-actual)]
         [vy0  (vel-vy vel-actual)]
         [techo (+ 25 grosor-pared)]
         [bottom (- alto-ventana 95)]
         [vy1 (+ vy0 gravedad)]
         [y1  (+ y0 vy1)]
         [y1c (min (max y1 techo) bottom)])

    (define bajando? (> vy1 0))

    (define-values (y2 vy2 en-suelo? golpe-techo?)
      (if bajando?
          (let-values ([(yy vyy suelo?) (pisar-plataformas x0 y0 y1c vy1)])
            (values yy vyy suelo? #f))
          (let-values ([(yy vyy golpe?) (golpear-techo x0 y0 y1c vy1)])
            (values yy vyy #f golpe?))))

    (define x1 (+ x0 vx0))
    (define x1c (max (+ pj-radio-x 15)
                     (min (- ancho-ventana pj-radio-x 15) x1)))

    ;; empujar cajas con el movimiento horizontal
    (define x1p (empujar-bloques x0 y2 x1c vx0))

    ;; bloquear contra paredes del mapa
    (define-values (x2 vx2)
      (bloquear-paredes x0 y2 x1p vx0))

    ;; fricción si está en suelo (incluye cajas)
    (define en-suelo-real? (sobre-algun-suelo? x2 y2))
    (define vx-fric (if en-suelo-real? (* vx2 friccion) vx2))
    (define vx-final
      (if (and en-suelo-real? (< (abs vx-fric) 0.5))
          0
          vx-fric))

    (estado (make-posn x2 y2)
            (vel vx-final vy2))))

;; =========================
;; CONTROLES
;; =========================
(define fuerza-salto 16)
(define salto-vx 6)
(define velocidad-max 15)

(define (mover mundo tecla)
  (define pos        (estado-pos mundo))
  (define vel-actual (estado-vel mundo))
  (define x  (posn-x pos))
  (define y  (posn-y pos))
  (define vx (vel-vx vel-actual))
  (define vy (vel-vy vel-actual))

  (define en-suelo-pos? (sobre-algun-suelo? x y))

  (cond
    ;; SALTO
    [(and (key=? tecla "up") en-suelo-pos?)
     (cond
       [(< vx 0)
        (estado pos (vel (- salto-vx) (- fuerza-salto)))]
       [(> vx 0)
        (estado pos (vel salto-vx (- fuerza-salto)))]
       [else
        (estado pos (vel 0 (- fuerza-salto)))]
       )]

    ;; IZQUIERDA
    [(key=? tecla "left")
     (estado pos
             (vel (max (- velocidad-max)
                       (- vx 2))
                  vy))]

    ;; DERECHA
    [(key=? tecla "right")
     (estado pos
             (vel (min velocidad-max
                       (+ vx 2))
                  vy))]

    [else mundo]))

;; =========================
;; BIG-BANG
;; =========================
(sonar-musica)
(big-bang estado-inicial
  [on-tick actualizar]
  [on-key mover]
  [to-draw dibujar])

