#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; Tamaño y física
(define ANCHO 800)
(define ALTO 600)
(define MITAD (/ ALTO 2))
(define GRAVEDAD 0.8)

;; Movilidad
(define VELOCIDAD-SALTO -13)
(define VELOCIDAD-MOVIMIENTO 5)

;; Geometría
(define J-ANCHO 16)
(define J-ALTO 48)
(define P-ALTO 15)

;; Pinchos
(define PINCHO-ALTURA 20)
(define PINCHO-ANCHO 20)
(define EPS 1)

;; Estructuras
(struct jugador (x y vx vy en-suelo) #:transparent)
(struct plataforma (x y ancho visible-arriba pinchos? pinchos-visibles?) #:transparent)
(struct mundo (estado jugador plataformas nivel rastros) #:transparent)

;; Bordes del jugador
(define (jugador-left j)  (- (jugador-x j) (/ J-ANCHO 2)))
(define (jugador-right j) (+ (jugador-x j) (/ J-ANCHO 2)))
(define (jugador-top j)   (- (jugador-y j) (/ J-ALTO 2)))
(define (jugador-bottom j) (+ (jugador-y j) (/ J-ALTO 2)))

;; Bordes de plataforma
(define (plat-left p)   (plataforma-x p))
(define (plat-right p)  (+ (plataforma-x p) (plataforma-ancho p)))
(define (plat-top p)    (- (plataforma-y p) (/ P-ALTO 2)))
(define (plat-bottom p) (+ (plataforma-y p) (/ P-ALTO 2)))

;; Top del bloque espejo
(define (plat-top-espejo p)
  (define y-espejo (+ MITAD (- MITAD (plataforma-y p))))
  (+ y-espejo (/ P-ALTO 2)))

;; --------------------------
;; Personaje sólido (negro arriba, gris abajo)
;; --------------------------
(define (dibujar-personaje opaco)
  (define color-base
    (if opaco
        (make-color 120 120 120) ; gris sólido
        (make-color 0 0 0)))     ; negro sólido
  (define cabeza (circle 8 "solid" color-base))
  (define cuerpo (rectangle 16 24 "solid" color-base))
  (define piernas
    (beside
     (rectangle 6 16 "solid" color-base)
     (rectangle 4 16 "solid" "transparent")
     (rectangle 6 16 "solid" color-base)))
  (above cabeza cuerpo piernas))

;; --------------------------
;; Personaje semitransparente (para rastros, ~30% opaco)
;; --------------------------
(define (dibujar-personaje-rastro opaco)
  (define color-base
    (if opaco
        (make-color 120 120 120 75) ; gris translúcido (30% opaco)
        (make-color 0 0 0 75)))     ; negro translúcido (30% opaco)
  (define cabeza (circle 8 "solid" color-base))
  (define cuerpo (rectangle 16 24 "solid" color-base))
  (define piernas
    (beside
     (rectangle 6 16 "solid" color-base)
     (rectangle 4 16 "solid" "transparent")
     (rectangle 6 16 "solid" color-base)))
  (above cabeza cuerpo piernas))

;; --------------------------
;; Rastro fugaz con forma del personaje
;; --------------------------
(define VIDA-RASTRO 8)

;; Agrega rastro si se mueve o salta
(define (agregar-rastro j rastros)
  (if (or (not (= (jugador-vx j) 0))
          (not (jugador-en-suelo j)))
      (cons (list (jugador-x j) (jugador-y j) VIDA-RASTRO) rastros)
      rastros))

;; Actualiza y elimina rastros que expiran
(define (actualizar-rastros rastros)
  (filter (lambda (r) (> (third r) 0))
          (map (lambda (r) (list (first r) (second r) (- (third r) 1))) rastros)))

;; Dibujo del rastro arriba (negro translúcido)
(define (dibujar-rastro-arriba r escena)
  (place-image (dibujar-personaje-rastro #f)
               (first r) (second r) escena))

;; Dibujo del rastro espejo (gris translúcido)
(define (dibujar-rastro-espejo r escena)
  (place-image (flip-vertical (dibujar-personaje-rastro #t))
               (first r) (+ MITAD (- MITAD (second r))) escena))

;; --------------------------
;; Datos iniciales
;; --------------------------
(define plataformas-iniciales
  (list (plataforma 0 270 800 #t #f #f)))

(define mundo-inicial
  (mundo "menu" (jugador 100 222 0 0 #f) plataformas-iniciales 0 '()))









;; Pantalla de menú
(define (dibujar-menu)
  (overlay
   (above
    (text "REFLECTION" 60 "black")
    (rectangle 10 30 "solid" "transparent")
    (text "Presiona ESPACIO para jugar" 20 "gray")
    (rectangle 10 20 "solid" "transparent")
    (text "Controles: ← → ↑" 18 "gray")
    (rectangle 10 20 "solid" "transparent")
    (text "Meta: Llegar al lado derecho" 18 "gray"))
   (empty-scene ANCHO ALTO "white")))

;; Pantalla de selección de nivel
(define (dibujar-seleccion)
  (overlay
   (above
    (text "SELECCIONA NIVEL" 50 "black")
    (rectangle 10 30 "solid" "transparent")
    (text "Presiona 1 para Nivel 1" 25 "gray")
    (text "Presiona 2 para Nivel 2" 25 "gray")
    (text "Presiona 3 para Nivel 3" 25 "gray"))
   (empty-scene ANCHO ALTO "white")))

;; Pantalla de victoria
(define (dibujar-victoria)
  (overlay
   (above
    (text "¡NIVEL COMPLETADO!" 50 "black")
    (rectangle 10 30 "solid" "transparent")
    (text "Presiona R para reiniciar" 20 "gray"))
   (empty-scene ANCHO ALTO "white")))

;; Posiciones x (izquierda) de cada pincho en la plataforma p
(define (spike-lefts p)
  (define start (plataforma-x p))
  (define end   (+ (plataforma-x p) (plataforma-ancho p)))
  (define (loop x acc)
    (if (>= x end)
        (reverse acc)
        (loop (+ x PINCHO-ANCHO) (cons x acc))))
  (loop start '()))

;; Dibuja pinchos individuales arriba (solo si son visibles)
(define (dibujar-pinchos-arriba p escena)
  (if (and (plataforma-pinchos? p) (plataforma-pinchos-visibles? p))
      (let ([y-top (plat-top p)]
            [color "black"])
        (foldl
         (lambda (sx scn)
           (place-image (triangle PINCHO-ALTURA "solid" color)
                        (+ sx (/ PINCHO-ANCHO 2))
                        (- y-top (/ PINCHO-ALTURA 2))
                        scn))
         escena
         (spike-lefts p)))
      escena))

;; Dibuja pinchos individuales en el reflejo (solo si son visibles)
(define (dibujar-pinchos-espejo p escena)
  (if (and (plataforma-pinchos? p) (plataforma-pinchos-visibles? p))
      (let ([y-top (plat-top-espejo p)]
            [color (make-color 150 150 150)])
        (foldl
         (lambda (sx scn)
           (place-image (flip-vertical (triangle PINCHO-ALTURA "solid" color))
                        (+ sx (/ PINCHO-ANCHO 2))
                        (+ y-top (/ PINCHO-ALTURA 2))
                        scn))
         escena
         (spike-lefts p)))
      escena))

;; Dibuja todas las plataformas (arriba y espejo)
(define (dibujar-todas-plataformas plats escena)
  (if (empty? plats)
      escena
      (dibujar-todas-plataformas
       (rest plats)
       (dibujar-plataforma-doble (first plats) escena))))

;; Dibuja una plataforma y su reflejo
(define (dibujar-plataforma-doble p escena)
  (define color-real "black")
  (define color-espejo (make-color 150 150 150))
  (define base-real (rectangle (plataforma-ancho p) P-ALTO "solid" color-real))
  (define base-espejo (rectangle (plataforma-ancho p) P-ALTO "solid" color-espejo))
  (define y-real (plataforma-y p))
  (define y-espejo (+ MITAD (- MITAD y-real)))
  (define x-centro (+ (plataforma-x p) (/ (plataforma-ancho p) 2)))

  ;; Espejo (abajo)
  (define escena-con-espejo (place-image base-espejo x-centro y-espejo escena))
  (define escena-con-pinchos-espejo (dibujar-pinchos-espejo p escena-con-espejo))

  ;; Arriba (real)
  (define escena-con-base-arriba
    (if (plataforma-visible-arriba p)
        (place-image base-real x-centro y-real escena-con-pinchos-espejo)
        escena-con-pinchos-espejo))

  (dibujar-pinchos-arriba p escena-con-base-arriba))

;; Intersección AABB (rectángulos)
(define (rect-overlap? l1 r1 t1 b1 l2 r2 t2 b2)
  (and (< l1 r2) (> r1 l2) (< t1 b2) (> b1 t2)))

;; Jugador intersecta con plataforma
(define (intersecta? j p)
  (rect-overlap? (jugador-left j) (jugador-right j) (jugador-top j) (jugador-bottom j)
                 (plat-left p) (plat-right p) (plat-top p) (plat-bottom p)))

;; Cajas de colisión de cada pincho (arriba)
(define (spike-boxes-arriba p)
  (map (lambda (sx)
         (list sx
               (min (+ sx PINCHO-ANCHO) (plat-right p))
               (- (plat-top p) PINCHO-ALTURA)
               (plat-top p)))
       (spike-lefts p)))

;; Cajas de colisión de cada pincho (espejo)
(define (spike-boxes-espejo p)
  (map (lambda (sx)
         (list sx
               (min (+ sx PINCHO-ANCHO) (plat-right p))
               (plat-top-espejo p)
               (+ (plat-top-espejo p) PINCHO-ALTURA)))
       (spike-lefts p)))

;; ¿Jugador toca cualquier pincho (arriba)?
(define (toca-spikes-arriba? j p)
  (and (plataforma-pinchos? p)
       (ormap (lambda (box)
                (define sl (first box))
                (define sr (second box))
                (define sb (third box))
                (define st (fourth box))
                (rect-overlap? (jugador-left j) (jugador-right j) (jugador-top j) (jugador-bottom j)
                               (- sl EPS) (+ sr EPS) sb st))
              (spike-boxes-arriba p))))

;; ¿Jugador toca cualquier pincho (espejo)?
(define (toca-spikes-espejo? j p)
  (and (plataforma-pinchos? p)
       (ormap (lambda (box)
                (define sl (first box))
                (define sr (second box))
                (define sb (third box))
                (define st (fourth box))
                (rect-overlap? (jugador-left j) (jugador-right j) (jugador-top j) (jugador-bottom j)
                               (- sl EPS) (+ sr EPS) sb st))
              (spike-boxes-espejo p))))

;; Apoyado exactamente sobre pincho (arriba)
(define (apoyado-sobre-spike-arriba? j p)
  (and (plataforma-pinchos? p)
       (ormap (lambda (box)
                (define sl (first box))
                (define sr (second box))
                (define sb (third box))
                (define st (fourth box))
                (and (<= (abs (- (jugador-bottom j) st)) 1)
                     (<= (jugador-right j) (+ sr EPS))
                     (>= (jugador-left j)  (- sl EPS))))
              (spike-boxes-arriba p))))

;; Apoyado exactamente sobre pincho (espejo)
(define (apoyado-sobre-spike-espejo? j p)
  (and (plataforma-pinchos? p)
       (ormap (lambda (box)
                (define sl (first box))
                (define sr (second box))
                (define sb (third box))
                (define st (fourth box))
                (and (<= (abs (- (jugador-top j) sb)) 1)
                     (<= (jugador-right j) (+ sr EPS))
                     (>= (jugador-left j)  (- sl EPS))))
              (spike-boxes-espejo p))))

;; Resolver movimiento horizontal
(define (resolver-horizontal j plats)
  (define nueva-x (max 20 (min (- ANCHO 20) (+ (jugador-x j) (jugador-vx j)))))
  (define j-mov (jugador nueva-x (jugador-y j) (jugador-vx j) (jugador-vy j) (jugador-en-suelo j)))
  (define (resolver-contra p acc-j)
    (if (intersecta? acc-j p)
        (cond
          [(> (jugador-vx acc-j) 0) ;; moviéndose a la derecha
           (jugador (- (plat-left p) (/ J-ANCHO 2))
                    (jugador-y acc-j) 0 (jugador-vy acc-j) (jugador-en-suelo acc-j))]
          [(< (jugador-vx acc-j) 0) ;; moviéndose a la izquierda
           (jugador (+ (plat-right p) (/ J-ANCHO 2))
                    (jugador-y acc-j) 0 (jugador-vy acc-j) (jugador-en-suelo acc-j))]
          [else acc-j])
        acc-j))
  (foldl resolver-contra j-mov plats))

;; Resolver movimiento vertical
(define (resolver-vertical j plats)
  (define nueva-vy (+ (jugador-vy j) GRAVEDAD))
  (define nueva-y (+ (jugador-y j) nueva-vy))
  (define j-mov (jugador (jugador-x j) nueva-y (jugador-vx j) nueva-vy #f))
  (define (resolver-contra p acc-j)
    (if (intersecta? acc-j p)
        (cond
          [(> (jugador-vy acc-j) 0) ;; cayendo
           (jugador (jugador-x acc-j)
                    (- (plat-top p) (/ J-ALTO 2))
                    (jugador-vx acc-j) 0 #t)]
          [(< (jugador-vy acc-j) 0) ;; subiendo
           (jugador (jugador-x acc-j)
                    (+ (plat-bottom p) (/ J-ALTO 2))
                    (jugador-vx acc-j) 0 #f)]
          [else acc-j])
        acc-j))
  (foldl resolver-contra j-mov plats))

;; Reflejar plataformas invisibles (para el espejo)
(define (reflejar-plataformas plats)
  (map (lambda (p)
         (plataforma
          (plataforma-x p)
          (+ MITAD (- MITAD (plataforma-y p)))
          (plataforma-ancho p)
          #t
          (plataforma-pinchos? p)
          (plataforma-pinchos-visibles? p)))
       (filter (lambda (p) (not (plataforma-visible-arriba p))) plats)))

;; --------------------------
;; Definición de niveles
;; --------------------------

(define plataformas-nivel1
  (list
   ;; Suelo principal
   (plataforma 0 270 800 #t #f #f)

   ;; Plataforma visible arriba
   (plataforma 200 200 120 #t #f #f)

   ;; Plataforma oculta arriba, visible en el espejo
   (plataforma 500 180 100 #f #f #f)

   ;; Nueva plataforma visible arriba
   (plataforma 350 140 80 #t #f #f)

   ;; Nueva plataforma solo en el espejo
   (plataforma 650 220 100 #f #f #f)

   ;; Pinchos en el suelo debajo de la plataforma de x=200
   (plataforma 200 270 120 #t #t #t)

   ;; Pinchos en el suelo debajo de la plataforma de x=350
   (plataforma 350 270 80 #t #t #t)

   ;; Pinchos en el suelo debajo de la plataforma de x=500
   (plataforma 500 270 100 #t #t #t)

   ;; Pinchos en el suelo debajo de la plataforma de x=650
   (plataforma 650 270 100 #t #t #t)))


(define plataformas-nivel2
  (list
   (plataforma 0 270 240 #t #f #f)
   (plataforma 240 270 40  #t #t #t)
   (plataforma 280 270 520 #t #t #t)
   (plataforma 300 180 150 #f #f #f)
   (plataforma 500 120 100 #t #f #f)
   (plataforma 620 160 60 #t #f #f)

   
   (plataforma 140 270 20 #t #t #t)
   (plataforma 160 270 20 #t #t #t)
   (plataforma 180 270 20 #t #t #t)))


(define plataformas-nivel3
  (list
    ;; Suelo principal
    (plataforma 0   270 260 #t #f #f)
    (plataforma 260 270  20 #t #t #t)
    (plataforma 280 270 460 #t #t #t)
    (plataforma 740 270  40 #t #t #t)     ;; piso que tapa el agujero
    (plataforma 780 270  20 #t #t #t)

    ;; Plataformas superiores
    (plataforma 150 200  20 #f #f #f)     ;; miniplataforma invisible
    (plataforma 170 200  60 #t #f #f)     ;; reaparece plataforma de soporte
    (plataforma 170 200  60 #f #t #t)     ;; pinchos flotantes encima
    (plataforma 250 200  20 #f #f #f)     ;; miniplataforma invisible

    (plataforma 400 150  20 #f #f #f)     ;; miniplataforma invisible
    (plataforma 420 150  40 #t #f #f)     ;; reaparece plataforma de soporte
    (plataforma 420 150  40 #t #t #t)     ;; pinchos flotantes encima
    (plataforma 480 150  20 #f #f #f)     ;; miniplataforma invisible

    (plataforma 600 100  20 #f #f #f)     ;; miniplataforma invisible
    (plataforma 620 100  60 #t #f #f)     ;; reaparece plataforma de soporte
    (plataforma 620 100  60 #t #t #t)     ;; pinchos flotantes encima
    (plataforma 660 100  20 #f #f #f)))   ;; miniplataforma invisible



;; Actualización del juego
(define (actualizar-juego m)
  (define j (mundo-jugador m))
  (define plats (mundo-plataformas m))
  (define todas-plataformas (append plats (reflejar-plataformas plats)))

  ;; mover por ejes
  (define j1 (resolver-horizontal j todas-plataformas))
  (define j2 (resolver-vertical j1 todas-plataformas))

  ;; actualizar rastros (se acumulan y desaparecen con VIDA-RASTRO)
  (define rastros-nuevos (actualizar-rastros (agregar-rastro j2 (mundo-rastros m))))

  ;; contacto con pinchos individuales (arriba y espejo)
  (define toca-pinchos?
    (ormap (lambda (p)
             (or (toca-spikes-arriba? j2 p)
                 (toca-spikes-espejo? j2 p)))
           plats))

  ;; caso especial: apoyado exactamente sobre un pincho (arriba o espejo)
  (define apoyado-arriba?
    (ormap (lambda (p) (apoyado-sobre-spike-arriba? j2 p)) plats))
  (define apoyado-espejo?
    (ormap (lambda (p) (apoyado-sobre-spike-espejo? j2 p)) plats))

  (cond
    ;; muerte por pinchos → reinicio del nivel actual, rastros vacíos
    [(or toca-pinchos? apoyado-arriba? apoyado-espejo?)
     (cond
       [(= (mundo-nivel m) 1)
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel1 1 '())]
       [(= (mundo-nivel m) 2)
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel2 2 '())]
       [(= (mundo-nivel m) 3)
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel3 3 '())]
       [else m])]

    ;; victoria → mantener rastros
    [(>= (jugador-right j2) (- ANCHO 20))
     (mundo "victoria" j2 plats (mundo-nivel m) rastros-nuevos)]

    ;; juego normal → actualizar jugador y rastros
    [else
     (mundo "jugando" j2 plats (mundo-nivel m) rastros-nuevos)]))

;; Manejo de teclas
(define (manejar-tecla m tecla)
  (cond
    ;; Menú → Selección
    [(and (string=? (mundo-estado m) "menu") (key=? tecla " "))
     (mundo "seleccion" (mundo-jugador m) (mundo-plataformas m) (mundo-nivel m) (mundo-rastros m))]

    ;; Selección → Niveles
    [(string=? (mundo-estado m) "seleccion")
     (cond
       [(key=? tecla "1")
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel1 1 '())]
       [(key=? tecla "2")
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel2 2 '())]
       [(key=? tecla "3")
        (mundo "jugando" (jugador 100 222 0 0 #f) plataformas-nivel3 3 '())]
       [else m])]

    ;; Victoria → Reinicio
    [(and (string=? (mundo-estado m) "victoria") (key=? tecla "r")) 
     mundo-inicial]

    ;; Juego normal
    [(string=? (mundo-estado m) "jugando")
     (define j (mundo-jugador m))
     (cond
       [(key=? tecla "left")
        (mundo "jugando"
               (jugador (jugador-x j) (jugador-y j)
                        (- VELOCIDAD-MOVIMIENTO) (jugador-vy j) (jugador-en-suelo j))
               (mundo-plataformas m) (mundo-nivel m) (mundo-rastros m))]
       [(key=? tecla "right")
        (mundo "jugando"
               (jugador (jugador-x j) (jugador-y j)
                        VELOCIDAD-MOVIMIENTO (jugador-vy j) (jugador-en-suelo j))
               (mundo-plataformas m) (mundo-nivel m) (mundo-rastros m))]
       [(and (key=? tecla "up") (jugador-en-suelo j))
        (mundo "jugando"
               (jugador (jugador-x j) (jugador-y j)
                        (jugador-vx j) VELOCIDAD-SALTO #f)
               (mundo-plataformas m) (mundo-nivel m) (mundo-rastros m))]
       [else m])]
    [else m]))

;; Soltar teclas
(define (soltar-tecla m tecla)
  (cond
    [(string=? (mundo-estado m) "jugando")
     (define j (mundo-jugador m))
     (cond
       [(or (key=? tecla "left") (key=? tecla "right"))
        ;; al soltar izquierda o derecha, se detiene el movimiento horizontal
        (mundo "jugando"
               (jugador (jugador-x j) (jugador-y j)
                        0 (jugador-vy j) (jugador-en-suelo j))
               (mundo-plataformas m) (mundo-nivel m) (mundo-rastros m))]
       [else m])]
    [else m]))

;; Actualización del mundo según estado
(define (actualizar-mundo m)
  (cond
    [(string=? (mundo-estado m) "jugando") (actualizar-juego m)]
    [else m]))

;; Dibujo del juego con rastros y espejo
(define (dibujar-juego m)
  (define j (mundo-jugador m))
  (define escena (empty-scene ANCHO ALTO "white"))
  (define escena-con-linea (add-line escena 0 MITAD ANCHO MITAD "black"))
  (define escena-con-plats (dibujar-todas-plataformas (mundo-plataformas m) escena-con-linea))

  ;; Dibujar rastros
  (define rastros (mundo-rastros m))
  (define escena-con-rastros-arriba (foldl dibujar-rastro-arriba escena-con-plats rastros))
  (define escena-con-rastros-espejo (foldl dibujar-rastro-espejo escena-con-rastros-arriba rastros))

  ;; Reflejo del jugador (gris opaco)
  (define escena-con-reflejo
    (place-image (flip-vertical (dibujar-personaje #t))
                 (jugador-x j) (+ MITAD (- MITAD (jugador-y j))) escena-con-rastros-espejo))

  ;; Jugador arriba (negro)
  (place-image (dibujar-personaje #f) (jugador-x j) (jugador-y j) escena-con-reflejo))

;; Selector de pantallas según estado
(define (dibujar-mundo m)
  (cond
    [(string=? (mundo-estado m) "menu") (dibujar-menu)]
    [(string=? (mundo-estado m) "seleccion") (dibujar-seleccion)]
    [(string=? (mundo-estado m) "jugando") (dibujar-juego m)]
    [(string=? (mundo-estado m) "victoria") (dibujar-victoria)]
    [else (empty-scene ANCHO ALTO "white")]))

;; Programa principal
(big-bang mundo-inicial
  (on-tick actualizar-mundo)
  (to-draw dibujar-mundo)
  (on-key manejar-tecla)
  (on-release soltar-tecla))