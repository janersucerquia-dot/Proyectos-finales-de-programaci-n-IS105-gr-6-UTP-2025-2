#lang racket

(require 2htdp/universe 2htdp/image)
(require (only-in racket/gui play-sound))
(require racket/runtime-path)
(define-runtime-path musica-fondo "soundtrack.wav")

;; -----------------------
;; Imágenes 
;; -----------------------

(define FONDO         (bitmap "fondo.png"))
(define PACMAN        (list (bitmap "pac1.png") (bitmap "pac2.png") (bitmap "pac3.png") (bitmap "pac2.png")))
(define PINK-GHOST    (list (bitmap "ghost1.png") (bitmap "ghost2.png")))
(define BLUE-GHOST    (list (bitmap "ghost-blue1.png") (bitmap "ghost-blue2.png")))
(define MENU1         (bitmap "menu1.png"))
(define MENU-GHOST    (bitmap "menughost.png"))
(define NIVEL1        (bitmap "nivel1.png"))
(define OBSTACULO1    (bitmap "triangulo.png"))
(define REINTENTAR    (bitmap "reintentar.png"))
(define NEXT          (bitmap "next.png"))
(define MENU2         (bitmap "menu2.png"))
(define NIVEL2        (bitmap "nivel2.png"))
(define PUERTA        (bitmap "puerta.png"))
(define MENU3         (bitmap "menu3.png"))
(define NIVEL3        (bitmap "nivel3.png"))
(define (empty-image w h) (rectangle w h "solid" "transparent"))
(define FIN           (bitmap "fin.png"))

;; -----------------------
;; World Struct
;; -----------------------

(struct world (estado pac ghost menu-ghost-x menu-ghost-y
                     pac-x pac-y ghost-x ghost-y
                     ghost2 ghost2-x ghost2-y ghost2-alive
                     jumping jump-t last-dx
                     obst1-x obst1-y
                     obst2-x obst2-y
                     puerta-x puerta-y
                     nivel-actual ghost-alive)
  #:transparent)

;; initial-world
(define initial-world
  (world "inicio"
         0 0 0 575
         150 600 850 600
         0 900 600 #f
         #f 0 0
         700 700
         700 700
         2000 2000
         "inicio"
         #t))

;; -----------------------
;; clicks / botones
;; -----------------------
(define (click-en-start? x y)
  (and (>= x 290) (<= x 646)
       (>= y 360) (<= y 478)))

(define NIVEL1-XMIN 150)
(define NIVEL1-XMAX 320)
(define NIVEL1-YMIN 230)
(define NIVEL1-YMAX 420)

(define NIVEL2-XMIN 415)
(define NIVEL2-XMAX 585)
(define NIVEL2-YMIN 230)
(define NIVEL2-YMAX 420)

(define NIVEL3-XMIN 650)
(define NIVEL3-XMAX 820)
(define NIVEL3-YMIN 230)
(define NIVEL3-YMAX 420)

(define (click-en-boton? x y x-min x-max y-min y-max)
  (and (>= x x-min) (<= x x-max)
       (>= y y-min) (<= y y-max)))

;; -----------------------
;; Inicializar niveles
;; -----------------------
(define (inicializar-nivel nivel-estado)
  (cond
    [(string=? nivel-estado "nivel1")
     (world "nivel1"
            0 0 0 575
            150 600 850 600
            0 900 600 #f
            #f 0 0
            400 600 2000 2000
            2000 2000
            "nivel1"
            #t)]
    [(string=? nivel-estado "nivel2")
     (world "nivel2"
            0 0 0 575
            150 600 850 600
            0 900 600 #f
            #f 0 0
            300 605 600 605
            800 610
            "nivel2"
            #t)]
    [(string=? nivel-estado "nivel3")
     (world "nivel3"
            0 0 0 575
            150 500 850 500
            0 700 605 #t
            #f 0 0
            300 598 400 598
            930 620
            "nivel3"
            #t)]
    [(string=? nivel-estado "menu2")
     (world "menu2"
            0 0 0 575
            (world-pac-x initial-world) (world-pac-y initial-world)
            (world-ghost-x initial-world) (world-ghost-y initial-world)
            0 900 600 #f
            #f 0 0
            400 600 2000 2000
            2000 2000
            "menu2"
            #t)]
    [(string=? nivel-estado "menu3")
     (world "menu3"
            0 0 0 575
            (world-pac-x initial-world) (world-pac-y initial-world)
            (world-ghost-x initial-world) (world-ghost-y initial-world)
            0 900 600 #f
            #f 0 0
            400 600 2000 2000
            2000 2000
            "menu3"
            #t)]
    [else (error "Nivel no reconocido" nivel-estado)]))

;; -----------------------
;; Colisiones
;; -----------------------
(define (colision-obstaculo? x y ox oy)
  (and (>= x (- ox 40)) (<= x (+ ox 40))
       (>= y (- oy 50)) (<= y (+ 10 oy))))

(define (colision-con-lista? w new-x new-y obst-list)
  (let ([margin-altura 40])
    (ormap (lambda (obs)
             (let ([ox (car obs)]
                   [oy (cdr obs)])
               (and (colision-obstaculo? new-x new-y ox oy)
                    (>= new-y (- oy margin-altura))
                    (not (world-jumping w)))))
           obst-list)))

(define (colision-puerta? pac-x pac-y puerta-x puerta-y)
  (let ([puerta-ancho 50]
        [puerta-alto 50])
    (and (>= pac-x (- puerta-x puerta-ancho)) (<= pac-x (+ puerta-x puerta-ancho))
         (>= pac-y (- puerta-y puerta-alto)) (<= pac-y (+ puerta-y 10)))))

;; -----------------------
;; Dibujar obstáculos
;; -----------------------
(define (dibujar-obstaculos w base)
  (let* ([estado (world-estado w)]
         [y-offset (cond
                     [(string=? estado "nivel2") -15]
                     [(string=? estado "nivel3") 0]
                     [else 0])]
         [img1 (place-image OBSTACULO1
                            (world-obst1-x w)
                            (+ (world-obst1-y w) y-offset)
                            base)]
         [img2 (if (or (string=? estado "nivel2")
                       (string=? estado "nivel3"))
                   (place-image OBSTACULO1
                                (world-obst2-x w)
                                (+ (world-obst2-y w) y-offset)
                                img1)
                   img1)])
    img2))

(define (obstaculos-activos w)
  (cond
    [(string=? (world-estado w) "nivel1") (list (cons (world-obst1-x w) (world-obst1-y w)))]
    [(string=? (world-estado w) "nivel2") (list (cons (world-obst1-x w) (world-obst1-y w)) (cons (world-obst2-x w) (world-obst2-y w)))]
    [(string=? (world-estado w) "nivel3") (list (cons (world-obst1-x w) (world-obst1-y w)) (cons (world-obst2-x w) (world-obst2-y w)))]
    [else '()]))

;; -----------------------
;; Tick
;; -----------------------
(define TICK-SPEED 0.1)
(define JUMP-DURATION 20)
(define JUMP-HEIGHT 120)

(define (tick w)
  (cond
    ;menu
    [(or (string=? (world-estado w) "inicio")
         (string=? (world-estado w) "menu1")
         (string=? (world-estado w) "menu2")
         (string=? (world-estado w) "menu3"))
     (let* ([temp-x (+ (world-menu-ghost-x w) 15)]
            [new-x (if (> temp-x 800) 0 temp-x)])
       (world (world-estado w)
              (modulo (+ 1 (world-pac w)) (length PACMAN))
              (modulo (+ 1 (world-ghost w)) (length PINK-GHOST))
              new-x (world-menu-ghost-y w)
              (world-pac-x w) (world-pac-y w)
              (world-ghost-x w) (world-ghost-y w)
              (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
              (world-jumping w) (world-jump-t w) (world-last-dx w)
              (world-obst1-x w) (world-obst1-y w)
              (world-obst2-x w) (world-obst2-y w)
              (world-puerta-x w) (world-puerta-y w)
              (world-nivel-actual w)
              (world-ghost-alive w)))]
    ;niveles
    [(or (string=? (world-estado w) "nivel1")
         (string=? (world-estado w) "nivel2")
         (string=? (world-estado w) "nivel3"))
     (let* ([jumping? (world-jumping w)]
            [t (world-jump-t w)]
            [ground-y 600]
            [dx (world-last-dx w)]
            [new-t (if jumping? (+ t 1) t)]
            [still-jumping? (and jumping? (< new-t JUMP-DURATION))]
            [final-t (if still-jumping? new-t 0)]
            [new-dx (if still-jumping? dx 0)]
            [r (if (zero? JUMP-DURATION) 0 (/ (exact->inexact new-t) JUMP-DURATION))]
            [new-y (if jumping? (- ground-y (* 4 JUMP-HEIGHT r (- 1 r))) ground-y)]
            [pac-x-move (if jumping? (* 2 dx) 0)]
            [new-pac-x (+ (world-pac-x w) pac-x-move)]
            [new-ghost-x (+ new-pac-x -100)]
            [new-ghost2-x (+ new-pac-x 300)]
            [obst-list (obstaculos-activos w)]
            [pac-coll? (colision-con-lista? w new-pac-x new-y obst-list)]
            [ghost1-coll?
             (and (world-ghost-alive w)
                  (ormap (lambda (obs)
                           (let ([ox (car obs)] [oy (cdr obs)])
                             (colision-obstaculo? (world-ghost-x w) (world-ghost-y w) ox oy)))
                         obst-list))]
            [ghost2-coll?
             (and (world-ghost2-alive w)
                  (ormap (lambda (obs)
                           (let ([ox (car obs)] [oy (cdr obs)])
                             (colision-obstaculo? (world-ghost2-x w) (world-ghost2-y w) ox oy)))
                         obst-list))]
            [puerta-coll? (colision-puerta? new-pac-x new-y (world-puerta-x w) (world-puerta-y w))])
       (cond
         ; PIERDE
         [pac-coll?
          (world "reintentar"
                 (world-pac w) (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 (world-pac-x w) ground-y
                 (world-ghost-x w) ground-y
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 #f 0 0
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 (world-ghost-alive w))]
         ;; Nivel1: fantasma toca obst
         [(and (string=? (world-estado w) "nivel1") ghost1-coll?)
          (world "menu2"
                 (world-pac w) (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 (world-pac-x w) (world-pac-y w)
                 (world-ghost-x w) (world-ghost-y w)
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 (world-jumping w) (world-jump-t w) (world-last-dx w)
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 "menu2"
                 (world-ghost-alive w))]
         ;; Nivel2: pacman llega puerta
         [(and (string=? (world-estado w) "nivel2") puerta-coll?)
          (world "next"
                 (world-pac w) (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 (world-pac-x w) (world-pac-y w)
                 (world-ghost-x w) (world-ghost-y w)
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 #f 0 0
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 (world-ghost-alive w))]
         ;; Nivel3: ambos fantasmas muertos + puerta
         [(and (string=? (world-estado w) "nivel3")
               (not (world-ghost-alive w))
               (not (world-ghost2-alive w))
               puerta-coll?)
          (world "fin"
                 (world-pac w) (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 (world-pac-x w) (world-pac-y w)
                 (world-ghost-x w) (world-ghost-y w)
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 #f 0 0
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 (world-ghost-alive w))]
         
         ;; Nivel2: fantasma1 muere
         [(and ghost1-coll? (string=? (world-estado w) "nivel2"))
          (world (world-estado w)
                 (modulo (+ 1 (world-pac w)) (length PACMAN))
                 (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 new-pac-x new-y
                 2000 new-y
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 still-jumping? final-t new-dx
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 #f)]
         ;; Nivel3: fantasma1 muere
         [(and ghost1-coll? (string=? (world-estado w) "nivel3"))
          (world (world-estado w)
                 (modulo (+ 1 (world-pac w)) (length PACMAN))
                 (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 new-pac-x new-y
                 2000 new-y
                 (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                 still-jumping? final-t new-dx
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 #f)]
         ;; Nivel3: fantasma2 muere 
         [(and ghost2-coll? (string=? (world-estado w) "nivel3"))
          (world (world-estado w)
                 (modulo (+ 1 (world-pac w)) (length PACMAN))
                 (world-ghost w)
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 new-pac-x new-y
                 (world-ghost-x w) (world-ghost-y w)
                 (world-ghost2 w) 2000 new-y #f   
                 still-jumping? final-t new-dx
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 (world-ghost-alive w))]
         
         [else
          (world (world-estado w)
                 (modulo (+ 1 (world-pac w)) (length PACMAN))
                 (modulo (+ 1 (world-ghost w)) (length PINK-GHOST))
                 (world-menu-ghost-x w) (world-menu-ghost-y w)
                 new-pac-x new-y
                 new-ghost-x new-y
                 (world-ghost2 w)
                 (if (world-ghost2-alive w) new-ghost2-x (world-ghost2-x w))
                 (if (world-ghost2-alive w) new-y (world-ghost2-y w))
                 (world-ghost2-alive w)
                 still-jumping? final-t new-dx
                 (world-obst1-x w) (world-obst1-y w)
                 (world-obst2-x w) (world-obst2-y w)
                 (world-puerta-x w) (world-puerta-y w)
                 (world-nivel-actual w)
                 (world-ghost-alive w))]))]
   ;otros estados
    [(or (string=? (world-estado w) "reintentar")
         (string=? (world-estado w) "next"))
     w]
    [else w]))

;; -----------------------
;; Mouse handler
;; -----------------------
(define (mouse-handler w x y event)
  (cond
    ; FIN: click anywhere 
    [(and (string=? (world-estado w) "fin")
          (string=? event "button-down"))
     initial-world]

    ; Inicio -> Menu1
    [(and (string=? (world-estado w) "inicio")
          (string=? event "button-down")
          (click-en-start? x y))
     (world "menu1"
            0 0 0 575
            (world-pac-x w) (world-pac-y w)
            (world-ghost-x w) (world-ghost-y w)
            (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
            #f 0 0
            (world-obst1-x w) (world-obst1-y w)
            (world-obst2-x w) (world-obst2-y w)
            (world-puerta-x w) (world-puerta-y w)
            "menu1"
            (world-ghost-alive w))]
    ; Botón nivel1
    [(and (or (string=? (world-estado w) "menu1")
              (string=? (world-estado w) "menu2")
              (string=? (world-estado w) "menu3"))
          (string=? event "button-down")
          (click-en-boton? x y NIVEL1-XMIN NIVEL1-XMAX NIVEL1-YMIN NIVEL1-YMAX))
     (inicializar-nivel "nivel1")]
    ; Botón nivel2
    [(and (or (string=? (world-estado w) "menu2")
              (string=? (world-estado w) "menu3"))
          (string=? event "button-down")
          (click-en-boton? x y NIVEL2-XMIN NIVEL2-XMAX NIVEL2-YMIN NIVEL2-YMAX))
     (inicializar-nivel "nivel2")]
    ; Botón nivel3 
    [(and (string=? (world-estado w) "menu3")
          (string=? event "button-down")
          (click-en-boton? x y NIVEL3-XMIN NIVEL3-XMAX NIVEL3-YMIN NIVEL3-YMAX))
     (inicializar-nivel "nivel3")]
    ; REINTENTAR: click anywhere
    [(and (string=? (world-estado w) "reintentar")
          (string=? event "button-down"))
     (inicializar-nivel (world-nivel-actual w))]
    ; NEXT: click anywhere 
    [(and (string=? (world-estado w) "next")
          (string=? event "button-down"))
     (let ([nivel (world-nivel-actual w)])
       (cond
         [(string=? nivel "nivel1") (inicializar-nivel "menu2")]
         [(string=? nivel "nivel2") (inicializar-nivel "menu3")]
         [(string=? nivel "nivel3") (inicializar-nivel "menu1")]
         [else (inicializar-nivel "menu1")] ))]
    [else w]))

;; -----------------------
;; Dibujar nivel
;; -----------------------
(define (dibujar-nivel w)
  (let* ([base (cond [(string=? (world-estado w) "nivel2") NIVEL2]
                     [(string=? (world-estado w) "nivel3") NIVEL3]
                     [else NIVEL1])]
         [with-obst (dibujar-obstaculos w base)]
         [with-puerta (if (< (world-puerta-x w) 1000)
                          (place-image PUERTA (world-puerta-x w) (world-puerta-y w) with-obst)
                          with-obst)]
         [char-y-offset (if (string=? (world-estado w) "nivel2") -10 0)]
         [pac-img (list-ref PACMAN (world-pac w))]
         [pac-y (+ (world-pac-y w) char-y-offset)]
         [ghost-img (list-ref PINK-GHOST (world-ghost w))]
         [ghost-y (+ (world-ghost-y w) char-y-offset)]
         [ghost2-img (if (string=? (world-estado w) "nivel3")
                         (list-ref BLUE-GHOST (world-ghost2 w))
                         (empty-image 1 1))]
         [ghost2-y (+ (world-ghost2-y w) char-y-offset)])
    (place-image pac-img (world-pac-x w) pac-y
      (place-image (if (world-ghost-alive w) ghost-img (empty-image 1 1))
                   (world-ghost-x w) ghost-y
        (place-image (if (world-ghost2-alive w) ghost2-img (empty-image 1 1))
                     (world-ghost2-x w) ghost2-y
                     with-puerta)))))

;; -----------------------
;; Draw principal
;; -----------------------
(define (draw w)
  (cond
    [(string=? (world-estado w) "inicio")
     (place-image (list-ref PACMAN (world-pac w)) 100 600
       (place-image (list-ref PINK-GHOST (world-ghost w)) 850 600
         FONDO))]
    [(string=? (world-estado w) "menu1")
     (place-image MENU-GHOST (world-menu-ghost-x w) (world-menu-ghost-y w) MENU1)]
    [(string=? (world-estado w) "menu2")
     (place-image MENU-GHOST (world-menu-ghost-x w) (world-menu-ghost-y w) MENU2)]
    [(string=? (world-estado w) "menu3")
     (place-image MENU-GHOST (world-menu-ghost-x w) (world-menu-ghost-y w) MENU3)]
    [(or (string=? (world-estado w) "nivel1")
         (string=? (world-estado w) "nivel2")
         (string=? (world-estado w) "nivel3"))
     (dibujar-nivel w)]
    [(string=? (world-estado w) "reintentar") REINTENTAR]
    [(string=? (world-estado w) "next") NEXT]
    [(string=? (world-estado w) "fin") FIN]
    [else FONDO]))

;; -----------------------
;; Teclado
;; -----------------------
(define (key-handler w key)
  (if (or (string=? (world-estado w) "nivel1")
           (string=? (world-estado w) "nivel2")
           (string=? (world-estado w) "nivel3"))
      (cond
        [(string=? key "right")
         (let* ([new-pac-x (+ (world-pac-x w) 5)]
                [new-ghost-x (+ new-pac-x -100)]
                [obst-list (obstaculos-activos w)]
                [pac-y-check (if (world-jumping w) (world-pac-y w) 600)])
           (if (colision-con-lista? w new-pac-x pac-y-check obst-list)
               w
               (world (world-estado w)
                      (world-pac w) (world-ghost w)
                      (world-menu-ghost-x w) (world-menu-ghost-y w)
                      new-pac-x (world-pac-y w)
                      new-ghost-x (world-ghost-y w)
                      (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                      (world-jumping w)
                      (world-jump-t w)
                      8
                      (world-obst1-x w) (world-obst1-y w)
                      (world-obst2-x w) (world-obst2-y w)
                      (world-puerta-x w) (world-puerta-y w)
                      (world-nivel-actual w)
                      (world-ghost-alive w))))]
        [(string=? key "left")
         (let* ([new-pac-x (- (world-pac-x w) 5)]
                [new-ghost-x (+ new-pac-x -100)]
                [obst-list (obstaculos-activos w)]
                [pac-y-check (if (world-jumping w) (world-pac-y w) 600)])
           (if (colision-con-lista? w new-pac-x pac-y-check obst-list)
               w
               (world (world-estado w)
                      (world-pac w) (world-ghost w)
                      (world-menu-ghost-x w) (world-menu-ghost-y w)
                      new-pac-x (world-pac-y w)
                      new-ghost-x (world-ghost-y w)
                      (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                      (world-jumping w)
                      (world-jump-t w)
                      -12
                      (world-obst1-x w) (world-obst1-y w)
                      (world-obst2-x w) (world-obst2-y w)
                      (world-puerta-x w) (world-puerta-y w)
                      (world-nivel-actual w)
                      (world-ghost-alive w))))]
        [(string=? key "up")
         (if (not (world-jumping w))
             (world (world-estado w)
                    (world-pac w) (world-ghost w)
                    (world-menu-ghost-x w) (world-menu-ghost-y w)
                    (world-pac-x w) (world-pac-y w)
                    (world-ghost-x w) (world-ghost-y w)
                    (world-ghost2 w) (world-ghost2-x w) (world-ghost2-y w) (world-ghost2-alive w)
                    #t
                    0
                    (world-last-dx w)
                    (world-obst1-x w) (world-obst1-y w)
                    (world-obst2-x w) (world-obst2-y w)
                    (world-puerta-x w) (world-puerta-y w)
                    (world-nivel-actual w)
                    (world-ghost-alive w))
             w)]
        [else w])
      w))

;;;; -----------------------
;; Musica
;; -----------------------

(define (iniciar-musica)
  (thread
   (lambda()
     (let loop()
       (play-sound musica-fondo #f)
       (sleep 0.1)
       (loop)))))

;; -----------------------
;; Big-bang
;; -----------------------
(iniciar-musica)
(big-bang initial-world
  [to-draw draw]
  [on-tick tick TICK-SPEED]
  [on-mouse mouse-handler]
  [on-key key-handler]
  [stop-when (lambda (w) #f)])
