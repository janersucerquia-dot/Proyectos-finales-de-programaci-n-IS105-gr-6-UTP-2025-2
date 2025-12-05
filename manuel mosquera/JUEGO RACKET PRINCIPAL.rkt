#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;; ===========================================================
;; NUEVA FUNCIÓN DE DIBUJO DEL PERSONAJE (ESTILO PIXEL ART)
;; ===========================================================
(define (dibujar-personaje-pixelado)
  (let* ([escala-pixel 4]
         [c-piel (make-color 255 200 150)]
         [c-sombrero "white"]
         [c-cinta "gold"]
         [c-pantalon (make-color 30 100 200)]
         [c-ojos "black"]
         [c-pies (make-color 101 67 33)]
         [pix (lambda (ancho alto color) 
                (rectangle (* ancho escala-pixel) (* alto escala-pixel) "solid" color))])
    (above
     (above
      (pix 6 2 c-sombrero)
      (overlay (pix 2 2 c-cinta) (pix 6 2 c-sombrero))
      (pix 10 2 c-sombrero))
     (overlay
      (above
       (beside (pix 1 1 c-ojos) (pix 2 1 c-piel) (pix 1 1 c-ojos))
       (pix 4 1 c-piel)
       (pix 2 1 c-ojos))
      (pix 6 5 c-piel))
     (pix 10 3 c-piel)
     (overlay (pix 2 2 c-cinta) (pix 6 2 c-pantalon))
     (pix 6 2 c-pantalon)
     (beside (pix 2 1 c-pies) (pix 2 1 "transparent") (pix 2 1 c-pies)))))

;; ==========================================
;; CONSTANTES
;; ==========================================
(define ANCHO 1200)
(define ALTO 600)
(define CENTRO-X (/ ANCHO 2))
(define GRAVEDAD 1.5)
(define VELOCIDAD-MOVIMIENTO 8)
(define FUERZA-SALTO -15)
(define TAMANO-CUBO 40)
(define VELOCIDAD-BALA 40)
(define GRAVEDAD-BALA 0.5)
(define RESISTENCIA-BALA 0.99)
(define MAX-BALAS 10)
(define TAMANO-BALA 8)

(define TITULO-INICIO-Y 150)
(define BOTON-PLAY-Y 350)
(define BOTON-ANCHO 300)
(define BOTON-ALTO 35)
(define TITULO-MENU-Y 150)
(define BOTON-CONTINUE-Y 300)
(define BOTON-NEWGAME-Y 380)
(define MENU-BOTON-ANCHO 300)
(define MENU-BOTON-ALTO 35)
(define NIVEL1-X 300)
(define NIVEL2-X 600)
(define NIVEL3-X 900)
(define BOTONES-NIVELES-Y 300)
(define NIVEL-BOTON-ANCHO 120)
(define NIVEL-BOTON-ALTO 120)
(define BOTON-BACK-Y 480)
(define BOTON-INGAME-ANCHO 150)
(define BOTON-INGAME-ALTO 40)

;; ==========================================
;; ESTRUCTURAS
;; ==========================================
(struct plataforma (x y ancho alto color) #:transparent)
(struct puerta (x y ancho alto) #:transparent)
(struct personaje (x y vx vy en-suelo?) #:transparent)
(struct bala (x y vx vy edad) #:transparent)
(struct enemigo (x y vivo?) #:transparent)
(struct cadena (x1 y1 x2 y2 rota?) #:transparent)
(struct obstaculo-colgante (x y vy cayendo?) #:transparent)
(struct globo (x y vy flotando?) #:transparent)
(struct datos-nivel (jugador plataformas puerta enemigos nivel-completado? nivel-fallado? balas 
                            cadena-nivel2 obstaculo-nivel2 cadena-nivel3 globo-nivel3) #:transparent)
(struct juego (estado tick-animacion nivel-actual datos musica-on sfx-on 
                      disparos-totales niveles-desbloqueados) #:transparent)

(define JUEGO-INICIAL (juego "inicio" 0 0 #f #t #t 0 1))

;; ==========================================
;; CREACIÓN DE NIVELES
;; ==========================================
(define (crear-nivel-1)
  (datos-nivel
    (personaje 320 340 0 0 #f)
    (list
      (plataforma 350 380 200 30 (make-color 210 180 140))
      (plataforma 550 410 200 30 (make-color 210 180 140))
      (plataforma 750 380 200 30 (make-color 210 180 140)))
    (puerta 780 323 60 80)
    (list)
    #f #f (list)
    #f #f #f #f))

(define (crear-nivel-2)
  (datos-nivel
   (personaje 420 380 0 0 #f)
   (list
    (plataforma 450 400 200 30 (make-color 210 180 140))
    (plataforma 600 110 200 30 (make-color 210 180 140))
    (plataforma 750 400 150 30 (make-color 210 180 140)))
   (puerta 760 345 60 80)
   (list)
   #f #f (list)
   (cadena 630 125 630 250 #f)
   (obstaculo-colgante 630 268 0 #f)
   #f #f))

(define (crear-nivel-3)
  (datos-nivel
   (personaje 350 380 0 0 #f)
   (list
    (plataforma 150 457 180 30 (make-color 210 180 140))
    (plataforma 350 400 120 30 (make-color 210 180 140))
    (plataforma 500 280 30 130 (make-color 210 180 140))
    (plataforma 580 100 120 30 (make-color 210 180 140))
    (plataforma 550 400 30 30 (make-color 210 180 140))
    (plataforma 700 400 150 30 (make-color 210 180 140)))
   (puerta 150 400 60 80)
   (list
    (enemigo 700 368 #t))
   #f #f (list)
   #f #f
   (cadena 550 330 550 400 #f)
   (globo 550 305 0 #t)))

;; ==========================================
;; FÍSICA
;; ==========================================
(define (colision? x1 y1 w1 h1 x2 y2 w2 h2)
  (and (< (abs (- x1 x2)) (+ (/ w1 2) (/ w2 2)))
       (< (abs (- y1 y2)) (+ (/ h1 2) (/ h2 2)))))

(define (colision-con-paredes? pj-x pj-y plataformas)
  (ormap (lambda (plat)
           (colision? pj-x pj-y TAMANO-CUBO TAMANO-CUBO
                      (plataforma-x plat) (plataforma-y plat)
                      (plataforma-ancho plat) (plataforma-alto plat)))
         plataformas))

(define (bala-colisiona-plataforma? b plataformas)
  (findf (lambda (plat)
           (colision? (bala-x b) (bala-y b) TAMANO-BALA TAMANO-BALA
                      (plataforma-x plat) (plataforma-y plat)
                      (plataforma-ancho plat) (plataforma-alto plat)))
         plataformas))

(define (bala-colisiona-enemigo? b enemigo)
  (and (enemigo-vivo? enemigo)
       (colision? (bala-x b) (bala-y b) TAMANO-BALA TAMANO-BALA
                  (enemigo-x enemigo) (enemigo-y enemigo) 40 40)))

(define (bala-colisiona-obstaculo? b obstaculo)
  (if (not obstaculo)
      #f
      (colision? (bala-x b) (bala-y b) TAMANO-BALA TAMANO-BALA
                 (obstaculo-colgante-x obstaculo) (obstaculo-colgante-y obstaculo) 30 30)))

(define (bala-colisiona-globo? b globo)
  (if (not globo)
      #f
      (colision? (bala-x b) (bala-y b) TAMANO-BALA TAMANO-BALA
                 (globo-x globo) (globo-y globo) 30 30)))

(define (bala-colisiona-cadena? b cadena)
  (if (or (not cadena) (cadena-rota? cadena))
      #f
      (let* ([bx (bala-x b)]
             [by (bala-y b)]
             [cx1 (cadena-x1 cadena)]
             [cy1 (cadena-y1 cadena)]
             [cx2 (cadena-x2 cadena)]
             [cy2 (cadena-y2 cadena)]
             [dx (- cx2 cx1)]
             [dy (- cy2 cy1)]
             [t (max 0 (min 1 (/ (+ (* (- bx cx1) dx) (* (- by cy1) dy))
                                  (+ (* dx dx) (* dy dy)))))]
             [px (+ cx1 (* t dx))]
             [py (+ cy1 (* t dy))]
             [dist (sqrt (+ (* (- bx px) (- bx px)) (* (- by py) (- by py))))])
        (< dist 5))))

(define (jugador-colisiona-enemigo? pj enemigo)
  #f)

(define (aplicar-gravedad-y-suelo pj plataformas)
  (let* ([vy-nueva (+ (personaje-vy pj) GRAVEDAD)]
         [x-tentativa (+ (personaje-x pj) (personaje-vx pj))]
         [y-tentativa (+ (personaje-y pj) vy-nueva)]
         [colision-horizontal? (colision-con-paredes? x-tentativa (personaje-y pj) plataformas)]
         [x-final (if colision-horizontal? (personaje-x pj) x-tentativa)]
         [suelo-encontrado
          (findf (lambda (plat)
                   (and
                    (<= (abs (- x-final (plataforma-x plat))) 
                        (+ (/ (plataforma-ancho plat) 2) (/ TAMANO-CUBO 2)))
                    (<= (- (personaje-y pj) (/ TAMANO-CUBO 2)) 
                        (- (plataforma-y plat) (/ (plataforma-alto plat) 2)))
                    (>= (+ y-tentativa (/ TAMANO-CUBO 2)) 
                        (- (plataforma-y plat) (/ (plataforma-alto plat) 2)))))
                 plataformas)])
    (if suelo-encontrado
        (personaje x-final
                   (- (- (plataforma-y suelo-encontrado) 
                         (/ (plataforma-alto suelo-encontrado) 2)) 
                      (/ TAMANO-CUBO 2))
                   (personaje-vx pj) 0 #t)
        (personaje x-final y-tentativa (personaje-vx pj) vy-nueva #f))))

(define (mover-bala b plataformas)
  (let* ([vx-current (bala-vx b)]
         [vy-temp (+ (bala-vy b) GRAVEDAD-BALA)]
         [x-new (+ (bala-x b) vx-current)]
         [y-new (+ (bala-y b) vy-temp)]
         [plat-colision (bala-colisiona-plataforma? 
                         (bala x-new y-new vx-current vy-temp 0) 
                         plataformas)]
         [vx-rebote (if (or (<= x-new (/ TAMANO-BALA 2)) 
                            (>= x-new (- ANCHO (/ TAMANO-BALA 2))))
                        (- vx-current)
                        vx-current)]
         [vy-rebote (if (or (<= y-new (/ TAMANO-BALA 2)) 
                            (>= y-new (- ALTO (/ TAMANO-BALA 2))))
                        (* -0.7 vy-temp)
                        vy-temp)]
         [vx-final (if plat-colision (* -0.8 vx-rebote) vx-rebote)]
         [vy-final (if plat-colision (* -0.8 vy-rebote) vy-rebote)]
         [vx-resist (* vx-final RESISTENCIA-BALA)]
         [vy-resist (* vy-final RESISTENCIA-BALA)])
    (bala (max (/ TAMANO-BALA 2) (min x-new (- ANCHO (/ TAMANO-BALA 2))))
          (max (/ TAMANO-BALA 2) (min y-new (- ALTO (/ TAMANO-BALA 2))))
          vx-resist vy-resist (+ (bala-edad b) 1))))

(define (actualizar-balas-y-enemigos balas enemigos plataformas obstaculo2 globo3)
  (let* ([balas-movidas (map (lambda (b) (mover-bala b plataformas)) balas)]
         ;; Procesar colisiones con enemigos
         [resultado-enemigos
          (foldl (lambda (b acc)
                   (let ([enemigos-acc (car acc)]
                         [balas-acc (cdr acc)])
                     (let ([enemigo-golpeado 
                            (findf (lambda (e) (bala-colisiona-enemigo? b e)) enemigos-acc)])
                       (if enemigo-golpeado
                           (cons (map (lambda (e)
                                        (if (and (= (enemigo-x e) (enemigo-x enemigo-golpeado))
                                                 (= (enemigo-y e) (enemigo-y enemigo-golpeado)))
                                            (enemigo (enemigo-x e) (enemigo-y e) #f)
                                            e))
                                      enemigos-acc)
                                 balas-acc)
                           (cons enemigos-acc (cons b balas-acc))))))
                 (cons enemigos '())
                 balas-movidas)]
         [enemigos-nuevos (car resultado-enemigos)]
         [balas-tras-enemigos (cdr resultado-enemigos)]
         
         ;; Procesar colisiones con obstáculo nivel 2
         [resultado-obstaculo
          (if obstaculo2
              (foldl (lambda (b acc)
                       (let ([obs-acc (car acc)]
                             [balas-acc (cdr acc)])
                         (if (and obs-acc (bala-colisiona-obstaculo? b obs-acc))
                             (cons #f balas-acc) ; Obstáculo destruido, bala consumida
                             (cons obs-acc (cons b balas-acc)))))
                     (cons obstaculo2 '())
                     balas-tras-enemigos)
              (cons #f balas-tras-enemigos))]
         [obstaculo-nuevo (car resultado-obstaculo)]
         [balas-tras-obstaculo (cdr resultado-obstaculo)]
         
         ;; Procesar colisiones con globo nivel 3
         [resultado-globo
          (if globo3
              (foldl (lambda (b acc)
                       (let ([glob-acc (car acc)]
                             [balas-acc (cdr acc)])
                         (if (and glob-acc (bala-colisiona-globo? b glob-acc))
                             (cons #f balas-acc) ; Globo destruido, bala consumida
                             (cons glob-acc (cons b balas-acc)))))
                     (cons globo3 '())
                     balas-tras-obstaculo)
              (cons #f balas-tras-obstaculo))]
         [globo-nuevo (car resultado-globo)]
         [balas-tras-globo (cdr resultado-globo)]
         
         [balas-filtradas
          (filter (lambda (b)
                    (and (< (bala-edad b) 300)
                         (> (abs (bala-vx b)) 0.5)))
                  balas-tras-globo)])
    (list balas-filtradas enemigos-nuevos obstaculo-nuevo globo-nuevo)))

(define (actualizar-nivel-logica datos)
  (if (or (datos-nivel-nivel-completado? datos)
          (datos-nivel-nivel-fallado? datos))
      datos
      (let* ([pj (datos-nivel-jugador datos)]
             [plats (datos-nivel-plataformas datos)]
             [pta (datos-nivel-puerta datos)]
             [enemigos-old (datos-nivel-enemigos datos)]
             [balas-old (datos-nivel-balas datos)]
             [cadena2 (datos-nivel-cadena-nivel2 datos)]
             [obstaculo2 (datos-nivel-obstaculo-nivel2 datos)]
             [cadena3 (datos-nivel-cadena-nivel3 datos)]
             [globo3 (datos-nivel-globo-nivel3 datos)]
             [pj-movido (aplicar-gravedad-y-suelo pj plats)]
             [balas-pre (map (lambda (b) (mover-bala b plats)) balas-old)]
             
             ;; Nivel 2: Detectar si bala rompió cadena
             [cadena2-rota? (and cadena2 
                                 (not (cadena-rota? cadena2))
                                 (ormap (lambda (b) (bala-colisiona-cadena? b cadena2)) balas-pre))]
             [cadena2-nueva (if cadena2-rota?
                                (cadena (cadena-x1 cadena2) (cadena-y1 cadena2) 
                                       (cadena-x2 cadena2) (cadena-y2 cadena2) #t)
                                cadena2)]
             
             ;; Actualizar obstáculo cayendo si cadena se rompió
             [obstaculo2-nuevo 
              (if (and obstaculo2 cadena2-nueva (cadena-rota? cadena2-nueva))
                  (let* ([obs-vy (+ (obstaculo-colgante-vy obstaculo2) GRAVEDAD)]
                         [obs-y-nueva (+ (obstaculo-colgante-y obstaculo2) obs-vy)]
                         [toco-suelo? (>= obs-y-nueva 550)])
                    (if toco-suelo?
                        (obstaculo-colgante (obstaculo-colgante-x obstaculo2) 550 0 #f)
                        (obstaculo-colgante (obstaculo-colgante-x obstaculo2) obs-y-nueva obs-vy #t)))
                  obstaculo2)]
             
             ;; Nivel 2: Detectar si el obstáculo cayó al suelo (falló el nivel)
             [nivel2-fallado? (and obstaculo2-nuevo 
                                   (not (obstaculo-colgante-cayendo? obstaculo2-nuevo))
                                   (>= (obstaculo-colgante-y obstaculo2-nuevo) 540))]
             
             ;; Nivel 3: Detectar si bala rompió cadena del globo
             [cadena3-rota? (and cadena3 
                                 (not (cadena-rota? cadena3))
                                 (ormap (lambda (b) (bala-colisiona-cadena? b cadena3)) balas-pre))]
             [cadena3-nueva (if cadena3-rota?
                                (cadena (cadena-x1 cadena3) (cadena-y1 cadena3) 
                                       (cadena-x2 cadena3) (cadena-y2 cadena3) #t)
                                cadena3)]
             
             ;; Actualizar globo flotando si cadena se rompió
             [globo3-pre 
              (if (and globo3 cadena3-nueva (cadena-rota? cadena3-nueva) (globo-flotando? globo3))
                  (let* ([glob-vy (- (globo-vy globo3) 0.5)]
                         [glob-y-nueva (+ (globo-y globo3) glob-vy)]
                         [toco-techo? (<= glob-y-nueva 115)])
                    (if toco-techo?
                        (globo (globo-x globo3) 115 0 #f)
                        (globo (globo-x globo3) glob-y-nueva glob-vy #t)))
                  globo3)]
             
             ;; Actualizar balas, enemigos, obstáculo y globo
             [resultado-balas 
              (actualizar-balas-y-enemigos 
               (filter (lambda (b) 
                         (not (or (and cadena2-rota? (bala-colisiona-cadena? b cadena2))
                                  (and cadena3-rota? (bala-colisiona-cadena? b cadena3)))))
                       balas-pre)
               enemigos-old plats obstaculo2-nuevo globo3-pre)]
             [balas-nuevas (list-ref resultado-balas 0)]
             [enemigos-nuevos (list-ref resultado-balas 1)]
             [obstaculo2-final (list-ref resultado-balas 2)]
             [globo3-final (list-ref resultado-balas 3)]
             
             [toca-puerta? 
              (colision? (personaje-x pj-movido) (personaje-y pj-movido) 
                         TAMANO-CUBO TAMANO-CUBO
                         (puerta-x pta) (puerta-y pta) 
                         (puerta-ancho pta) (puerta-alto pta))]
             
             ;; Nivel 1: todos los enemigos muertos (no hay enemigos, así que siempre True)
             ;; Nivel 2: obstáculo destruido (la cadena no es necesaria, solo ayuda)
             ;; Nivel 3: globo destruido Y todos los enemigos muertos (la cadena no es necesaria, solo ayuda)
             [objetivo-completado?
              (cond
                [(datos-nivel-obstaculo-nivel2 datos)
                 ; Nivel 2: solo obstáculo destruido
                 (not obstaculo2-final)]
                [(datos-nivel-globo-nivel3 datos)
                 ; Nivel 3: globo destruido Y enemigos muertos
                 (and (not globo3-final)
                      (andmap (lambda (e) (not (enemigo-vivo? e))) enemigos-nuevos))]
                [else 
                 ; Nivel 1: solo enemigos (no hay, así que siempre True)
                 (andmap (lambda (e) (not (enemigo-vivo? e))) enemigos-nuevos)])])
        
        (datos-nivel pj-movido plats pta enemigos-nuevos 
                     (and toca-puerta? objetivo-completado?)
                     nivel2-fallado?
                     balas-nuevas
                     cadena2-nueva obstaculo2-final cadena3-nueva globo3-final))))

;; ==========================================
;; DIBUJO
;; ==========================================
(define (dibujar-fondo-desierto tick)
  (let* ([cielo (rectangle ANCHO ALTO "solid" (make-color 255 140 50))]
         [nube1-x (modulo (+ 350 (* tick 1)) ANCHO)]
         [nube2-x (modulo (+ 750 (* tick 1)) ANCHO)]
         [nube-pixel1
          (above
           (beside (rectangle 12 8 "solid" (make-color 240 240 240))
                   (rectangle 24 8 "solid" (make-color 245 245 245))
                   (rectangle 12 8 "solid" (make-color 240 240 240)))
           (beside (rectangle 8 8 "solid" (make-color 235 235 235))
                   (rectangle 36 8 "solid" (make-color 250 250 250))
                   (rectangle 8 8 "solid" (make-color 235 235 235)))
           (beside (rectangle 16 8 "solid" (make-color 240 240 240))
                   (rectangle 24 8 "solid" (make-color 245 245 245))))]
         [nube-pixel2
          (above
           (beside (rectangle 16 8 "solid" (make-color 240 240 240))
                   (rectangle 20 8 "solid" (make-color 245 245 245)))
           (beside (rectangle 8 8 "solid" (make-color 235 235 235))
                   (rectangle 28 8 "solid" (make-color 250 250 250))
                   (rectangle 8 8 "solid" (make-color 235 235 235)))
           (beside (rectangle 12 8 "solid" (make-color 240 240 240))
                   (rectangle 20 8 "solid" (make-color 245 245 245))))]
         [cielo-con-nubes
          (place-image nube-pixel1 nube1-x 80
                       (place-image nube-pixel2 nube2-x 150 cielo))]
         [montana-pixel-izq
          (above
           (rectangle 40 20 "solid" (make-color 110 75 40))
           (beside (rectangle 80 25 "solid" (make-color 110 75 40))
                   (rectangle 120 25 "solid" (make-color 101 67 33))
                   (rectangle 80 25 "solid" (make-color 110 75 40)))
           (beside (rectangle 60 30 "solid" (make-color 115 80 45))
                   (rectangle 160 30 "solid" (make-color 101 67 33))
                   (rectangle 60 30 "solid" (make-color 115 80 45)))
           (rectangle 280 35 "solid" (make-color 101 67 33)))]
         [montana-pixel-der
          (above
           (beside (rectangle 50 25 "solid" (make-color 115 80 45))
                   (rectangle 40 25 "solid" (make-color 110 75 40))
                   (rectangle 50 25 "solid" (make-color 115 80 45)))
           (beside (rectangle 60 30 "solid" (make-color 120 85 50))
                   (rectangle 140 30 "solid" (make-color 101 67 33))
                   (rectangle 60 30 "solid" (make-color 120 85 50)))
           (beside (rectangle 40 35 "solid" (make-color 115 80 45))
                   (rectangle 180 35 "solid" (make-color 101 67 33))
                   (rectangle 40 35 "solid" (make-color 115 80 45)))
           (beside (rectangle 30 40 "solid" (make-color 120 85 50))
                   (rectangle 220 40 "solid" (make-color 101 67 33))
                   (rectangle 30 40 "solid" (make-color 120 85 50))))]
         [brazo-base (rectangle 20 4 "solid" (make-color 11 59 14))]
         [Cactus1-Brazos
          (above
           (beside 
            (rectangle 10 8 "solid" (make-color 11 59 14))
            (rectangle 8 4 "solid" (make-color 120 80 35))
            (rectangle 10 8 "solid" (make-color 11 59 14)))
           brazo-base)]
         [Cactus2-Brazos
          (above
           (beside 
            (rectangle 12 10 "solid" (make-color 11 59 14))
            (rectangle 6 4 "solid" (make-color 120 80 35))
            (rectangle 12 10 "solid" (make-color 11 59 14)))
           brazo-base)]
         [Cactus3-Brazos
          (above
           (beside 
            (rectangle 14 12 "solid" (make-color 11 59 14))
            (rectangle 4 4 "solid" (make-color 120 80 35))
            (rectangle 14 12 "solid" (make-color 11 59 14)))
           brazo-base)]
         [cactus1 (above
                   (rectangle 8 30 "solid" (make-color 11 59 14))
                   (rectangle 5 18 "solid" (make-color 120 80 35)))]
         [cactus2 (above
                   (rectangle 8 30 "solid" (make-color 11 59 14))
                   (rectangle 5 18 "solid" (make-color 120 80 35)))]
         [cactus3 (above
                   (rectangle 9 32 "solid" (make-color 11 59 14))
                   (rectangle 6 19 "solid" (make-color 120 80 35)))]
         [suelo-desierto
          (above
           (rectangle ANCHO 100 "solid" (make-color 200 150 80))
           (rectangle ANCHO 50 "solid" (make-color 180 130 60))
           (rectangle ANCHO 30 "solid" (make-color 160 110 40)))]
         [escena-con-montanas
          (place-image montana-pixel-der 900 450
                       (place-image montana-pixel-izq 200 430
                                    (place-image suelo-desierto 
                                                 (/ ANCHO 2) (- ALTO 50)
                                                 cielo-con-nubes)))])
    (place-image cactus3 950 520
                 (place-image Cactus3-Brazos 950 508
                              (place-image cactus2 720 510
                                           (place-image Cactus2-Brazos 720 498
                                                        (place-image cactus1 80 515
                                                                     (place-image Cactus1-Brazos 80 503
                                                                                  escena-con-montanas))))))))

(define (icono-musica activo)
  (if activo
      (overlay (text "♪" 25 "yellow")
               (square 40 "solid" "black")
               (square 42 "outline" "yellow"))
      (overlay (text "♪" 25 "gray")
               (rotate 45 (rectangle 50 3 "solid" "red"))
               (square 40 "solid" "black")
               (square 42 "outline" "gray"))))

(define (icono-sfx activo)
  (if activo
      (overlay (text "♫" 15 "yellow")
               (square 40 "solid" "black")
               (square 42 "outline" "yellow"))
      (overlay (text "♫" 15 "gray")
               (square 40 "solid" "black")
               (square 42 "outline" "gray"))))

(define (dibujar-plataforma p fondo)
  (place-image (rectangle (plataforma-ancho p) (plataforma-alto p) 
                          "solid" (plataforma-color p))
               (plataforma-x p) (plataforma-y p) fondo))

(define (dibujar-bala b fondo)
  (let* ([color-naranja (make-color 255 100 0)]
         [imagen-bala 
          (overlay
           (rectangle TAMANO-BALA TAMANO-BALA "solid" color-naranja)
           (rectangle (+ TAMANO-BALA 2) (+ TAMANO-BALA 2) "outline" "black"))])
    (place-image imagen-bala (bala-x b) (bala-y b) fondo)))

(define (dibujar-cadena c fondo)
  (if (or (not c) (cadena-rota? c))
      fondo
      (let* ([x1 (cadena-x1 c)]
             [y1 (cadena-y1 c)]
             [x2 (cadena-x2 c)]
             [y2 (cadena-y2 c)]
             [num-segmentos 8]
             [dx (/ (- x2 x1) num-segmentos)]
             [dy (/ (- y2 y1) num-segmentos)])
        (foldl (lambda (i escena)
                 (let ([xi (+ x1 (* i dx))]
                       [yi (+ y1 (* i dy))]
                       [xf (+ x1 (* (+ i 1) dx))]
                       [yf (+ y1 (* (+ i 1) dy))])
                   (add-line escena xi yi xf yf (make-color 80 80 80))))
               fondo
               (range num-segmentos)))))

(define (dibujar-obstaculo-colgante obs fondo)
  (if obs
      (place-image 
       (overlay
        (circle 8 "solid" (make-color 84 13 3))
        (rectangle 30 30 "solid" "red")
        (rectangle 32 32 "outline" "black"))
       (obstaculo-colgante-x obs) (obstaculo-colgante-y obs) fondo)
      fondo))

(define (dibujar-globo glob fondo)
  (if glob
      (place-image 
       (above
        (overlay
         (circle 15 "solid" "lightblue")
         (circle 17 "outline" "black"))
        (rectangle 2 5 "solid" "gray"))
       (globo-x glob) (globo-y glob) fondo)
      fondo))

(define (dibujar-enemigo e fondo)
  (if (enemigo-vivo? e)
      (place-image 
       (overlay
        (circle 8 "solid" (make-color 84 13 3))
        (rectangle 30 30 "solid" "red")
        (rectangle 32 32 "outline" "black"))
       (enemigo-x e) (enemigo-y e) fondo)
      fondo))

(define (dibujar-puerta-con-estado datos pta)
  (let* ([objetivo-completado?
          (cond
            [(datos-nivel-obstaculo-nivel2 datos)
             ; Nivel 2: solo obstáculo destruido
             (not (datos-nivel-obstaculo-nivel2 datos))]
            [(datos-nivel-globo-nivel3 datos)
             ; Nivel 3: globo destruido Y enemigos muertos
             (and (not (datos-nivel-globo-nivel3 datos))
                  (andmap (lambda (e) (not (enemigo-vivo? e))) (datos-nivel-enemigos datos)))]
            [else
             ; Nivel 1: no hay obstáculos, siempre abierta
             #t])]
         [color-puerta (if objetivo-completado? 
                           (make-color 255 140 0)  ; Naranja cuando está abierta
                           (make-color 120 120 120))] ; Gris cuando está cerrada
         [puerta-base (rectangle (puerta-ancho pta) (puerta-alto pta) "solid" color-puerta)]
         [puerta-con-borde (overlay
                            (rectangle (+ (puerta-ancho pta) 2) (+ (puerta-alto pta) 2) "outline" "black")
                            puerta-base)])
    (if objetivo-completado?
        puerta-con-borde
        (overlay
         (above
          (circle 8 "solid" "black")
          (rectangle 6 12 "solid" "black"))
         puerta-con-borde))))

(define (dibujar-juego-activo j)
  (let* ([datos (juego-datos j)]
         [pj (datos-nivel-jugador datos)]
         [pta (datos-nivel-puerta datos)]
         [fondo (dibujar-fondo-desierto (juego-tick-animacion j))]
         [imagen-puerta (dibujar-puerta-con-estado datos pta)])
    (cond
      [(datos-nivel-nivel-completado? datos)
       (overlay 
        (above
         (text "LEVEL COMPLETED" 50 "yellow")
         (text " " 20 "black")
         (overlay (text "NEXT LEVEL" 20 "yellow")
                  (rectangle 200 40 "solid" "black")
                  (rectangle 204 44 "outline" "yellow"))
         (text " " 10 "black")
         (overlay (text "RESTART" 20 "yellow")
                  (rectangle 200 40 "solid" "black")
                  (rectangle 204 44 "outline" "yellow"))
         (text " " 10 "black")
         (overlay (text "LEVELS MENU" 20 "yellow")
                  (rectangle 200 40 "solid" "black")
                  (rectangle 204 44 "outline" "yellow")))
        fondo)]
      [(datos-nivel-nivel-fallado? datos)
       (overlay
        (above
         (text "LEVEL FAILED" 50 "red")
         (text " " 20 "black")
         (overlay (text "RESTART" 20 "yellow")
                  (rectangle 200 40 "solid" "black")
                  (rectangle 204 44 "outline" "yellow"))
         (text " " 10 "black")
         (overlay (text "LEVELS MENU" 20 "yellow")
                  (rectangle 200 40 "solid" "black")
                  (rectangle 204 44 "outline" "yellow")))
        fondo)]
      [else
       (let* ([escena-con-puerta (place-image imagen-puerta (puerta-x pta) (puerta-y pta) fondo)]
              [escena-con-plats (foldl dibujar-plataforma escena-con-puerta (datos-nivel-plataformas datos))]
              [escena-con-cadena2 (dibujar-cadena (datos-nivel-cadena-nivel2 datos) escena-con-plats)]
              [escena-con-obs2 (dibujar-obstaculo-colgante (datos-nivel-obstaculo-nivel2 datos) escena-con-cadena2)]
              [escena-con-cadena3 (dibujar-cadena (datos-nivel-cadena-nivel3 datos) escena-con-obs2)]
              [escena-con-globo3 (dibujar-globo (datos-nivel-globo-nivel3 datos) escena-con-cadena3)]
              [escena-con-enemigos (foldl dibujar-enemigo escena-con-globo3 (datos-nivel-enemigos datos))]
              [escena-con-balas (foldl dibujar-bala escena-con-enemigos (datos-nivel-balas datos))]
              [escena-con-jugador
               (place-image 
                (dibujar-personaje-pixelado)
                (personaje-x pj) (personaje-y pj)
                escena-con-balas)]
              [escena-con-contador
               (place-image 
                (text (string-append "SHOTS:" (number->string (juego-disparos-totales j)))
                      20 "white")
                80 30 escena-con-jugador)]
              [espacio-entre-botones 40]
              [total-ancho-botones (+ BOTON-INGAME-ANCHO BOTON-INGAME-ANCHO espacio-entre-botones)]
              [posicion-x-inicial (/ (- ANCHO total-ancho-botones) 2)]
              [BOTON-RESTART-X-CENTRADO (+ posicion-x-inicial (/ BOTON-INGAME-ANCHO 2))]
              [BOTON-MENU-X-CENTRADO (+ BOTON-RESTART-X-CENTRADO BOTON-INGAME-ANCHO espacio-entre-botones)]
              [BOTON-Y-FINAL (- ALTO 60)]
              [escena-con-boton-restart
               (place-image
                (overlay (text "RESTART" 18 "yellow")
                         (rectangle BOTON-INGAME-ANCHO BOTON-INGAME-ALTO "solid" "black")
                         (rectangle (+ BOTON-INGAME-ANCHO 4) (+ BOTON-INGAME-ALTO 4) "outline" "yellow"))
                BOTON-RESTART-X-CENTRADO BOTON-Y-FINAL
                escena-con-contador)]
              [escena-final
               (place-image
                (overlay (text "MENU" 18 "yellow")
                         (rectangle BOTON-INGAME-ANCHO BOTON-INGAME-ALTO "solid" "black")
                         (rectangle (+ BOTON-INGAME-ANCHO 4) (+ BOTON-INGAME-ALTO 4) "outline" "yellow"))
                BOTON-MENU-X-CENTRADO BOTON-Y-FINAL
                escena-con-boton-restart)])
         escena-final)])))

(define (dibujar-inicio tick)
  (place-image (text "CUBOB" 100 "yellow") CENTRO-X TITULO-INICIO-Y
               (place-image (overlay (text "Play" 25 "white") 
                                     (rectangle BOTON-ANCHO BOTON-ALTO "outline" "black"))
                            CENTRO-X BOTON-PLAY-Y (dibujar-fondo-desierto tick))))

(define (dibujar-menu tick nivel-actual musica-on sfx-on)
  (let* ([fondo (dibujar-fondo-desierto tick)]
         [titulo (overlay (text "CUBOB" 80 "orange") (text "CUBOB" 80 (make-color 255 100 0)))]
         [boton-continue
          (if (> nivel-actual 0)
              (overlay (text "CONTINUE" 25 "yellow")
                       (rectangle MENU-BOTON-ANCHO MENU-BOTON-ALTO "solid" "black")
                       (rectangle (+ MENU-BOTON-ANCHO 4) (+ MENU-BOTON-ALTO 4) "outline" "yellow"))
              (overlay (text "CONTINUE" 25 "gray")
                       (rectangle MENU-BOTON-ANCHO MENU-BOTON-ALTO "solid" (make-color 50 50 50))
                       (rectangle (+ MENU-BOTON-ANCHO 4) (+ MENU-BOTON-ALTO 4) "outline" "gray")))]
         [boton-newgame
          (overlay (text "NEW GAME" 25 "yellow")
                   (rectangle MENU-BOTON-ANCHO MENU-BOTON-ALTO "solid" "black")
                   (rectangle (+ MENU-BOTON-ANCHO 4) (+ MENU-BOTON-ALTO 4) "outline" "yellow"))]
         [icono-mus (icono-musica musica-on)]
         [icono-sfx-img (icono-sfx sfx-on)])
    (place-image icono-sfx-img (- ANCHO 50) 40
                 (place-image icono-mus (- ANCHO 110) 40
                              (place-image boton-newgame CENTRO-X BOTON-NEWGAME-Y
                                           (place-image boton-continue CENTRO-X BOTON-CONTINUE-Y
                                                        (place-image titulo CENTRO-X TITULO-MENU-Y
                                                                     fondo)))))))

(define (dibujar-caja-nivel num desbloqueado? completado?)
  (overlay
   (if completado?
       (text "★" 40 "yellow")
       (text (number->string num) 50 (if desbloqueado? "white" "gray")))
   (rectangle NIVEL-BOTON-ANCHO NIVEL-BOTON-ALTO "solid" 
              (if desbloqueado? "gray" (make-color 80 80 80)))
   (rectangle (+ NIVEL-BOTON-ANCHO 4) (+ NIVEL-BOTON-ALTO 4) "outline" 
              (if desbloqueado? "yellow" "gray"))))

(define (dibujar-seleccion tick niveles-desbloqueados disparos)
  (let* ([fondo (dibujar-fondo-desierto tick)]
         [titulo (text "CUBOB" 60 "orange")]
         [subtitulo (text (string-append "TOTAL SHOTS:" (number->string disparos)) 
                          25 "white")]
         [nivel1 (dibujar-caja-nivel 1 #t (>= niveles-desbloqueados 2))]
         [nivel2 (dibujar-caja-nivel 2 (>= niveles-desbloqueados 2) (>= niveles-desbloqueados 3))]
         [nivel3 (dibujar-caja-nivel 3 (>= niveles-desbloqueados 3) #f)]
         [boton-back
          (overlay (text "MAIN MENU" 20 "yellow")
                   (rectangle 250 40 "solid" "black")
                   (rectangle 254 44 "outline" "yellow"))])
    (place-image boton-back CENTRO-X BOTON-BACK-Y
                 (place-image nivel3 NIVEL3-X BOTONES-NIVELES-Y
                              (place-image nivel2 NIVEL2-X BOTONES-NIVELES-Y
                                           (place-image nivel1 NIVEL1-X BOTONES-NIVELES-Y
                                                        (place-image subtitulo CENTRO-X 220
                                                                     (place-image titulo CENTRO-X 150 fondo))))))))

(define (renderizar j)
  (cond
    [(string=? (juego-estado j) "inicio") 
     (dibujar-inicio (juego-tick-animacion j))]
    [(string=? (juego-estado j) "menu") 
     (dibujar-menu (juego-tick-animacion j) 
                   (juego-nivel-actual j)
                   (juego-musica-on j)
                   (juego-sfx-on j))]
    [(string=? (juego-estado j) "seleccion-niveles") 
     (dibujar-seleccion (juego-tick-animacion j) 
                        (juego-niveles-desbloqueados j)
                        (juego-disparos-totales j))]
    [(string=? (juego-estado j) "jugando") 
     (dibujar-juego-activo j)]
    [else (empty-scene ANCHO ALTO)]))

;; ==========================================
;; ACTUALIZACIÓN
;; ==========================================
(define (actualizar-juego j)
  (cond
    [(string=? (juego-estado j) "jugando")
     (juego "jugando"
            (+ (juego-tick-animacion j) 1)
            (juego-nivel-actual j)
            (actualizar-nivel-logica (juego-datos j))
            (juego-musica-on j)
            (juego-sfx-on j)
            (juego-disparos-totales j)
            (juego-niveles-desbloqueados j))]
    [else
     (juego (juego-estado j) 
            (+ (juego-tick-animacion j) 1) 
            (juego-nivel-actual j)
            (juego-datos j)
            (juego-musica-on j)
            (juego-sfx-on j)
            (juego-disparos-totales j)
            (juego-niveles-desbloqueados j))]))

;; ==========================================
;; MANEJO DE EVENTOS
;; ==========================================
(define (dentro? x y cx cy w h)
  (and (>= x (- cx (/ w 2))) (<= x (+ cx (/ w 2))) 
       (>= y (- cy (/ h 2))) (<= y (+ cy (/ h 2)))))

(define (manejar-clic j x y evento)
  (if (string=? evento "button-down")
      (cond
        [(string=? (juego-estado j) "inicio")
         (if (dentro? x y CENTRO-X BOTON-PLAY-Y BOTON-ANCHO BOTON-ALTO)
             (struct-copy juego j [estado "menu"]) j)]
        [(string=? (juego-estado j) "menu")
         (cond
           [(and (> (juego-nivel-actual j) 0)
                 (dentro? x y CENTRO-X BOTON-CONTINUE-Y MENU-BOTON-ANCHO MENU-BOTON-ALTO))
            (let ([nivel-datos (cond [(= (juego-nivel-actual j) 1) (crear-nivel-1)]
                                     [(= (juego-nivel-actual j) 2) (crear-nivel-2)]
                                     [(= (juego-nivel-actual j) 3) (crear-nivel-3)]
                                     [else (crear-nivel-1)])])
              (struct-copy juego j [estado "jugando"] [datos nivel-datos]))]
           [(dentro? x y CENTRO-X BOTON-NEWGAME-Y MENU-BOTON-ANCHO MENU-BOTON-ALTO)
            (struct-copy juego j [estado "seleccion-niveles"])]
           [(dentro? x y (- ANCHO 110) 40 42 42)
            (struct-copy juego j [musica-on (not (juego-musica-on j))])]
           [(dentro? x y (- ANCHO 50) 40 42 42)
            (struct-copy juego j [sfx-on (not (juego-sfx-on j))])]
           [else j])]
        [(string=? (juego-estado j) "seleccion-niveles")
         (cond
           [(dentro? x y NIVEL1-X BOTONES-NIVELES-Y NIVEL-BOTON-ANCHO NIVEL-BOTON-ALTO)
            (struct-copy juego j [estado "jugando"] [nivel-actual 1] [datos (crear-nivel-1)])]
           [(and (>= (juego-niveles-desbloqueados j) 2)
                 (dentro? x y NIVEL2-X BOTONES-NIVELES-Y NIVEL-BOTON-ANCHO NIVEL-BOTON-ALTO))
            (struct-copy juego j [estado "jugando"] [nivel-actual 2] [datos (crear-nivel-2)])]
           [(and (>= (juego-niveles-desbloqueados j) 3)
                 (dentro? x y NIVEL3-X BOTONES-NIVELES-Y NIVEL-BOTON-ANCHO NIVEL-BOTON-ALTO))
            (struct-copy juego j [estado "jugando"] [nivel-actual 3] [datos (crear-nivel-3)])]
           [(dentro? x y CENTRO-X BOTON-BACK-Y 250 40)
            (struct-copy juego j [estado "menu"])]
           [else j])]
        [(string=? (juego-estado j) "jugando")
         (let ([datos (juego-datos j)])
           (cond
             [(datos-nivel-nivel-completado? datos)
              (cond
                [(dentro? x y CENTRO-X 290 200 40)
                 (let* ([siguiente-nivel (+ (juego-nivel-actual j) 1)]
                        [nuevo-desbloqueado (max (juego-niveles-desbloqueados j) siguiente-nivel)]
                        [nivel-datos (cond [(= siguiente-nivel 2) (crear-nivel-2)]
                                           [(= siguiente-nivel 3) (crear-nivel-3)]
                                           [else #f])])
                   (if (<= siguiente-nivel 3)
                       (struct-copy juego j [estado "jugando"] [nivel-actual siguiente-nivel] 
                                    [datos nivel-datos] [niveles-desbloqueados nuevo-desbloqueado])
                       (struct-copy juego j [estado "seleccion-niveles"] [niveles-desbloqueados nuevo-desbloqueado])))]
                [(dentro? x y CENTRO-X 340 200 40)
                 (let ([nivel-datos (cond [(= (juego-nivel-actual j) 1) (crear-nivel-1)]
                                          [(= (juego-nivel-actual j) 2) (crear-nivel-2)]
                                          [(= (juego-nivel-actual j) 3) (crear-nivel-3)]
                                          [else (crear-nivel-1)])])
                   (struct-copy juego j [datos nivel-datos]))]
                [(dentro? x y CENTRO-X 390 200 40)
                 (struct-copy juego j [estado "seleccion-niveles"])]
                [else j])]
             [(datos-nivel-nivel-fallado? datos)
              (cond
                [(dentro? x y CENTRO-X 290 200 40)
                 (let ([nivel-datos (cond [(= (juego-nivel-actual j) 1) (crear-nivel-1)]
                                          [(= (juego-nivel-actual j) 2) (crear-nivel-2)]
                                          [(= (juego-nivel-actual j) 3) (crear-nivel-3)]
                                          [else (crear-nivel-1)])])
                   (struct-copy juego j [datos nivel-datos]))]
                [(dentro? x y CENTRO-X 340 200 40)
                 (struct-copy juego j [estado "seleccion-niveles"])]
                [else j])]
             [else
              (let* ([espacio-entre-botones 40]
                     [total-ancho-botones (+ BOTON-INGAME-ANCHO BOTON-INGAME-ANCHO espacio-entre-botones)]
                     [posicion-x-inicial (/ (- ANCHO total-ancho-botones) 2)]
                     [BOTON-RESTART-X-CENTRADO (+ posicion-x-inicial (/ BOTON-INGAME-ANCHO 2))]
                     [BOTON-MENU-X-CENTRADO (+ BOTON-RESTART-X-CENTRADO BOTON-INGAME-ANCHO espacio-entre-botones)]
                     [BOTON-Y-FINAL (- ALTO 60)])
                (cond
                  [(dentro? x y BOTON-RESTART-X-CENTRADO BOTON-Y-FINAL BOTON-INGAME-ANCHO BOTON-INGAME-ALTO)
                   (let ([nivel-datos (cond [(= (juego-nivel-actual j) 1) (crear-nivel-1)]
                                            [(= (juego-nivel-actual j) 2) (crear-nivel-2)]
                                            [(= (juego-nivel-actual j) 3) (crear-nivel-3)]
                                            [else (crear-nivel-1)])])
                     (struct-copy juego j [datos nivel-datos]))]
                  [(dentro? x y BOTON-MENU-X-CENTRADO BOTON-Y-FINAL BOTON-INGAME-ANCHO BOTON-INGAME-ALTO)
                   (struct-copy juego j [estado "seleccion-niveles"])]
                  [else
                   (let* ([pj (datos-nivel-jugador datos)]
                          [balas-actuales (datos-nivel-balas datos)])
                     (if (< (length balas-actuales) MAX-BALAS)
                         (let* ([pj-x (personaje-x pj)]
                                [pj-y (personaje-y pj)]
                                [dx (- x pj-x)]
                                [dy (- y pj-y)]
                                [distancia (sqrt (+ (* dx dx) (* dy dy)))]
                                [vx (* (/ dx distancia) VELOCIDAD-BALA)]
                                [vy (* (/ dy distancia) VELOCIDAD-BALA)]
                                [nueva-bala (bala pj-x pj-y vx vy 0)]
                                [nuevos-datos (struct-copy datos-nivel datos 
                                                           [balas (cons nueva-bala balas-actuales)])])
                           (struct-copy juego j [datos nuevos-datos] 
                                        [disparos-totales (+ (juego-disparos-totales j) 1)]))
                         j))]))]))]
        [else j])
      j))

(define (manejar-tecla j tecla)
  (cond
    [(string=? (juego-estado j) "jugando")
     (let* ([datos (juego-datos j)]
            [pj (datos-nivel-jugador datos)])
       (cond
         [(key=? tecla "escape")
          (struct-copy juego j [estado "menu"])]
         [(or (key=? tecla "left") (key=? tecla "a"))
          (let ([nuevo-pj (struct-copy personaje pj [vx (- VELOCIDAD-MOVIMIENTO)])])
            (struct-copy juego j [datos (struct-copy datos-nivel datos [jugador nuevo-pj])]))]
         [(or (key=? tecla "right") (key=? tecla "d"))
          (let ([nuevo-pj (struct-copy personaje pj [vx VELOCIDAD-MOVIMIENTO])])
            (struct-copy juego j [datos (struct-copy datos-nivel datos [jugador nuevo-pj])]))]
         [(and (or (key=? tecla "up") (key=? tecla "w") (key=? tecla " ")) 
               (personaje-en-suelo? pj))
          (let ([nuevo-pj (struct-copy personaje pj [vy FUERZA-SALTO] [en-suelo? #f])])
            (struct-copy juego j [datos (struct-copy datos-nivel datos [jugador nuevo-pj])]))]
         [else j]))]
    [(key=? tecla "escape") 
     (struct-copy juego j [estado "menu"])]
    [else j]))

(define (manejar-tecla-up j tecla)
  (if (string=? (juego-estado j) "jugando")
      (let* ([datos (juego-datos j)]
             [pj (datos-nivel-jugador datos)])
        (cond
          [(or (key=? tecla "left") (key=? tecla "right")
               (key=? tecla "a") (key=? tecla "d"))
           (let ([nuevo-pj (struct-copy personaje pj [vx 0])])
             (struct-copy juego j [datos (struct-copy datos-nivel datos [jugador nuevo-pj])]))]
          [else j]))
      j))

(big-bang JUEGO-INICIAL
  [to-draw renderizar]
  [on-tick actualizar-juego 0.03]
  [on-mouse manejar-clic]
  [on-key manejar-tecla]
  [on-release manejar-tecla-up])