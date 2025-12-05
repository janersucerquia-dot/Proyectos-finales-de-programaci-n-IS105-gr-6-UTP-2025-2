#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; IMÁGENES
(define imagen-menu (bitmap/file "play.jpg"))
(define imagen-personaje (bitmap/file "personaje.png"))
(define mapa1 (bitmap/file "mapa1.jpg"))
(define mapa2 (bitmap/file "mapa2.jpg"))

;; TAMAÑO PERSONAJE
(define personaje-ancho 20)
(define personaje-alto 32)

;; CONFIGURACIÓN
(define ancho 600)
(define alto 600)

;; PUERTA (mapa1 -> mapa2)
(define puerta-x 517)
(define puerta-y 77)
(define puerta-ancho 33)
(define puerta-alto 44)

(define (bbox-puerta)
  (list puerta-x
        puerta-y
        (+ puerta-x puerta-ancho)
        (+ puerta-y puerta-alto)))

;; PLATAFORMAS (solo mapa1)
(define plataforma3-x 174)
(define plataforma3-y 557)
(define plataforma3-ancho 251)
(define plataforma3-alto 43)

(define plataforma4-x 192)
(define plataforma4-y 512)
(define plataforma4-ancho 216)
(define plataforma4-alto 44)

(define plataforma5-x 0)
(define plataforma5-y 122)
(define plataforma5-ancho 174)
(define plataforma5-alto 478)

(define plataforma6-x 428)
(define plataforma6-y 122)
(define plataforma6-ancho 172)
(define plataforma6-alto 478)

(define plataforma7-x 268)
(define plataforma7-y 446)
(define plataforma7-ancho 115)
(define plataforma7-alto 154)

(define plataforma8-x 200)
(define plataforma8-y 122)
(define plataforma8-ancho 31)
(define plataforma8-alto 290)

(define plataforma9-x 375)
(define plataforma9-y 122)
(define plataforma9-ancho 31)
(define plataforma9-alto 290)

(define plataforma10-x 233)
(define plataforma10-y 20)
(define plataforma10-ancho 137)
(define plataforma10-alto 180)

(define lista-plataformas
  (list
   (list plataforma3-x plataforma3-y plataforma3-ancho plataforma3-alto)
   (list plataforma4-x plataforma4-y plataforma4-ancho plataforma4-alto)
   (list plataforma5-x plataforma5-y plataforma5-ancho plataforma5-alto)
   (list plataforma6-x plataforma6-y plataforma6-ancho plataforma6-alto)
   (list plataforma7-x plataforma7-y plataforma7-ancho plataforma7-alto)
   (list plataforma8-x plataforma8-y plataforma8-ancho plataforma8-alto)
   (list plataforma9-x plataforma9-y plataforma9-ancho plataforma9-alto)
   (list plataforma10-x plataforma10-y plataforma10-ancho plataforma10-alto)))

;; ESCALERAS (solo mapa1)
(define escalera1-x 175)
(define escalera1-y 125)
(define escalera1-ancho 20)
(define escalera1-alto 390)

(define escalera2-x 410)
(define escalera2-y 125)
(define escalera2-ancho 20)
(define escalera2-alto 390)

(define lista-escaleras
  (list
   (list escalera1-x escalera1-y escalera1-ancho escalera1-alto)
   (list escalera2-x escalera2-y escalera2-ancho escalera2-alto)))

;; ESTADO DEL JUEGO
(struct juego (x y vy suelo? mapa puerta-timer) #:transparent)

(define inicio-menu 'menu)
(define inicio-juego (juego 60 90 0 #f 'm1 0))

;; DIBUJADO
(define (dibujar-plataforma p escena)
  (place-image (rectangle (third p) (fourth p) "outline" "red")
               (+ (first p) (/ (third p) 2))
               (+ (second p) (/ (fourth p) 2))
               escena))

(define (dibujar-escalera e escena)
  (place-image (rectangle (third e) (fourth e) "outline" "green")
               (+ (first e) (/ (third e) 2))
               (+ (second e) (/ (fourth e) 2))
               escena))

(define (dibujar-puerta escena)
  (place-image (rectangle puerta-ancho puerta-alto "outline" "blue")
               (+ puerta-x (/ puerta-ancho 2))
               (+ puerta-y (/ puerta-alto 2))
               escena))

(define (dibujar-mundo s)
  (cond
    [(eq? (juego-mapa s) 'm1)
     (let* ([escena mapa1]
            [escena2 (foldl dibujar-plataforma escena lista-plataformas)]
            [escena3 (foldl dibujar-escalera escena2 lista-escaleras)]
            [escena4 (dibujar-puerta escena3)])
       (place-image imagen-personaje (juego-x s) (juego-y s) escena4))]
    [(eq? (juego-mapa s) 'm2)
     (place-image imagen-personaje (juego-x s) (juego-y s) mapa2)]
    [else
     (empty-scene ancho alto)]))

(define (dibujar-menu s)
  (place-image imagen-menu (/ ancho 2) (/ alto 2)
               (empty-scene ancho alto)))

;; BBOX / UTILIDADES COLISIÓN
(define (bbox-personaje x y)
  (list (- x (/ personaje-ancho 2))
        (- y (/ personaje-alto 2))
        (+ x (/ personaje-ancho 2))
        (+ y (/ personaje-alto 2))))

(define (bbox-plataforma p)
  (list (first p)
        (second p)
        (+ (first p) (third p))
        (+ (second p) (fourth p))))

(define (bbox-escalera e) (bbox-plataforma e))

;; devuelve #t si rectángulos se solapan (incluye tocar bordes)
(define (rect-superpone? a b)
  (let ([al (first a)] [at (second a)] [ar (third a)] [ab (fourth a)]
        [bl (first b)] [bt (second b)] [br (third b)] [bb (fourth b)])
    (and (<= al br) (>= ar bl) (<= at bb) (>= ab bt))))

;; ESCALERAS (solo mapa1)
(define (en-escalera? s)
  (and (juego? s)
       (eq? (juego-mapa s) 'm1)
       (for/or ([e lista-escaleras])
         (rect-superpone? (bbox-personaje (juego-x s) (juego-y s))
                          (bbox-escalera e)))))

;; COLISIONES MAPA1
(define gravedad 2)

(define (resolver-colisiones x y vy)
  (let ([px x] [py y] [pv vy])
    (for ([p lista-plataformas])
      (let* ([caja-plataforma (bbox-plataforma p)]
             [caja-personaje (bbox-personaje px py)])
        (when (rect-superpone? caja-personaje caja-plataforma)
          (let* ([pl (first caja-plataforma)]
                 [pt (second caja-plataforma)]
                 [pr (third caja-plataforma)]
                 [pb (fourth caja-plataforma)]
                 [plx (first caja-personaje)]
                 [pty (second caja-personaje)]
                 [prx (third caja-personaje)]
                 [pby (fourth caja-personaje)]
                 [dist-izq (abs (- prx pl))]
                 [dist-der (abs (- plx pr))]
                 [dist-arr (abs (- pby pt))]
                 [dist-aba (abs (- pty pb))]
                 [m (min dist-izq dist-der dist-arr dist-aba)])
            (cond
              [(= m dist-arr)
               (set! py (- pt (/ personaje-alto 2)))
               (set! pv 0)]
              [(= m dist-aba)
               (set! py (+ pb (/ personaje-alto 2)))
               (set! pv 0)]
              [(= m dist-izq)
               (set! px (- pl (/ personaje-ancho 2)))]
              [(= m dist-der)
               (set! px (+ pr (/ personaje-ancho 2)))])))))

    (values px py pv)))

;; PUERTA MAPA1 -> MAPA2
(define (en-puerta? s)
  (and (juego? s)
       (rect-superpone?
        (bbox-personaje (juego-x s) (juego-y s))
        (bbox-puerta))))

;; FÍSICA MAPA2
(define (tick-mapa2 s)
  ;; s guaranteed juego? and map 'm2
  (let* ([vy (+ (juego-vy s) gravedad)]
         [ny (+ (juego-y s) vy)]
         [nx (juego-x s)]
         [mitad (/ personaje-ancho 2)]
         [suelo-y (- alto (/ personaje-alto 2))])
    ;; colisión con suelo
    (when (>= ny suelo-y)
      (set! ny suelo-y)
      (set! vy 0))
    ;; límites horizontales
    (when (< nx mitad) (set! nx mitad))
    (when (> nx (- ancho mitad)) (set! nx (- ancho mitad)))
    ;; techo
    (when (< ny (/ personaje-alto 2))
      (set! ny (/ personaje-alto 2))
      (set! vy 0))
    (define en-suelo? (= vy 0))
    (juego nx ny vy en-suelo? 'm2 0)))

;; FÍSICA MAPA1
(define (tick-mapa1 s)
  (let* ([escalera? (en-escalera? s)]
         [vy (if escalera? 0 (+ (juego-vy s) gravedad))]
         [cand-x (juego-x s)]
         [cand-y (if escalera? (juego-y s) (+ (juego-y s) vy))]
         [suelo-y (- alto (/ personaje-alto 2))])
    ;; suelo global
    (when (>= cand-y suelo-y)
      (set! cand-y suelo-y)
      (set! vy 0))
    ;; resolver colisiones con plataformas
    (define-values (rx ry rvy)
      (resolver-colisiones cand-x cand-y vy))
    ;; límites X
    (define mitad (/ personaje-ancho 2))
    (set! rx (max mitad (min (- ancho mitad) rx)))
    ;; estado suelo
    (define en-suelo? (and (not escalera?) (= rvy 0)))
    ;; timer puerta (se incrementa si está dentro)
    (define nuevo-timer
      (if (en-puerta? s) (+ (juego-puerta-timer s) 1) 0))
    (if (>= nuevo-timer 56)
        ;; cambiar a mapa2 con posición inicial
        (juego 60 90 0 #f 'm2 0)
        (juego rx ry rvy en-suelo? 'm1 nuevo-timer))))

;; TICK GENERAL
(define (tick-juego s)
  (cond
    [(not (juego? s)) s]
    [(eq? (juego-mapa s) 'm2) (tick-mapa2 s)]
    [else (tick-mapa1 s)]))

;; TECLAS
(define (manejar-teclas s tecla)
  (cond
    [(not (juego? s)) s]
    [(eq? (juego-mapa s) 'm2)
     (cond
       [(key=? tecla "left")
        (juego (- (juego-x s) 10) (juego-y s) (juego-vy s) (juego-suelo? s) 'm2 (juego-puerta-timer s))]
       [(key=? tecla "right")
        (juego (+ (juego-x s) 10) (juego-y s) (juego-vy s) (juego-suelo? s) 'm2 (juego-puerta-timer s))]
       [(and (key=? tecla "up") (juego-suelo? s))
        (juego (juego-x s) (juego-y s) -18 #f 'm2 (juego-puerta-timer s))]
       [else s])]
    [else
     (cond
       [(key=? tecla "left")
        (juego (- (juego-x s) 10) (juego-y s) (juego-vy s) (juego-suelo? s) 'm1 (juego-puerta-timer s))]
       [(key=? tecla "right")
        (juego (+ (juego-x s) 10) (juego-y s) (juego-vy s) (juego-suelo? s) 'm1 (juego-puerta-timer s))]
       [(key=? tecla "up")
        (if (en-escalera? s)
            (juego (juego-x s) (- (juego-y s) 5) 0 #f 'm1 (juego-puerta-timer s))
            (if (juego-suelo? s)
                (juego (juego-x s) (juego-y s) -18 #f 'm1 (juego-puerta-timer s))
                s))]
       [(key=? tecla "down")
        (if (en-escalera? s)
            (juego (juego-x s) (+ (juego-y s) 5) 0 #f 'm1 (juego-puerta-timer s))
            s)]
       [else s])]))

;; CLICK MENÚ
(define (manejar-click s x y evento)
  (cond
    [(and (eq? s 'menu) (string=? evento "button-down"))
     (define mx (/ ancho 2))
     (define my (/ alto 2))
     (define w (image-width imagen-menu))
     (define h (image-height imagen-menu))
     (if (and (>= x (- mx (/ w 2)))
              (<= x (+ mx (/ w 2)))
              (>= y (- my (/ h 2)))
              (<= y (+ my (/ h 2))))
         inicio-juego
         s)]
    [else s]))

;; BIG-BANG
(big-bang inicio-menu
  (to-draw (lambda (s)
             (cond
               [(eq? s 'menu) (dibujar-menu s)]
               [(juego? s) (dibujar-mundo s)]
               [else (dibujar-menu s)])))
  (on-tick tick-juego)
  (on-key manejar-teclas)
  (on-mouse manejar-click))

