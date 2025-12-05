#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;                                PROYECTO: SAVE US
;                                JHON KEVIN MORALES
;                                 SECCIÓN 1
;                         CONFIGURACIÓN DEL SISTEMA

; 1.1 DIMENSIONES DE PANTALLA (FULL SCREEN HD)

(define ANCHO 1366)
(define ALTO 768)

; 1.2 CONSTANTES DEL MUNDO

; Ajustado para que el suelo quede abajo en tu pantalla
(define PISO-Y 710) 

; La meta está casi al final de la pantalla ancha
(define META-X 1300)

(define MAX-NIVELES 3)

; 1.3 PALETA DE COLORES

(define COLOR-FONDO "black")
(define COLOR-TEXTO "white")
(define COLOR-HUD "white")
(define COLOR-PELIGRO "red")
(define COLOR-EXITO "green")

; 1.4 MOTOR DE FÍSICA

(define GRAVEDAD 2.5)

(define SALTO -22)

(define VELOCIDAD 15)

(define VELOCIDAD-TITULO 3)


;                                 SECCIÓN 2
;                        MOTOR GRÁFICO (PIXEL ART)

(define (render-pixel-art matriz pixel-size color-primario color-secundario)
  (define (dibujar-celda val)
    (cond
      [(= val 0) (square pixel-size "solid" (make-color 0 0 0 0))]
      [(= val 1) (square pixel-size "solid" color-primario)]
      [(= val 2) (square pixel-size "solid" color-secundario)]
      [else (square pixel-size "solid" "red")]))
  
  (define (dibujar-fila fila) 
    (apply beside (map dibujar-celda fila)))
  
  (apply above (map dibujar-fila matriz)))

;                                 SECCIÓN 3
;                      BIBLIOTECA DE SPRITES (PERSONAJES)

; FANTASMA: ESTADO QUIETO (IDLE)
(define GHOST-IDLE-GRID
  (list
   '(0 0 1 1 1 0 0 0 0) 
   '(0 1 0 0 0 1 0 0 0)
   '(1 2 2 2 2 2 1 0 0)
   '(1 1 2 2 2 2 2 1 0) 
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 1 1 1 2 2 1) 
   '(0 0 1 1 0 0 1 1 0)
   '(0 0 1 0 0 0 1 0 0)))

; FANTASMA: ESTADO CAMINANDO (WALK)
(define GHOST-WALK-GRID
  (list
   '(0 0 1 1 1 0 0 0 0)
   '(0 1 0 0 0 1 0 0 0)
   '(1 2 2 2 2 2 1 0 0)
   '(1 1 2 2 2 2 2 1 0)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 0 1 1 1 1 1 0 0)
   '(0 0 0 1 0 1 0 0 0) 
   '(0 0 0 1 0 1 0 0 0)))

; FANTASMA: ESTADO RESPIRANDO (BREATHE)
(define GHOST-BREATHE-GRID
  (list
   '(0 0 1 1 1 0 0 0 0)
   '(0 1 0 0 0 1 0 0 0)
   '(1 2 2 2 2 2 1 0 0)
   '(1 1 2 2 2 2 2 1 0)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 1 2 1 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 2 2 2 2 2 1)
   '(0 1 2 1 1 1 2 2 1)
   '(0 0 1 1 0 0 1 1 0)
   '(0 0 1 0 0 0 1 0 0)))

; Convertimos las matrices en imágenes una sola vez al inicio.
(define RAW-IDLE (render-pixel-art GHOST-IDLE-GRID 4 "white" "black"))
(define RAW-BREATHE (render-pixel-art GHOST-BREATHE-GRID 4 "white" "black"))
(define RAW-WALK (render-pixel-art GHOST-WALK-GRID 4 "white" "black"))

; Caja de referencia transparente para mantener el centro de gravedad.
(define CAJA-REF (rectangle 40 65 "solid" "transparent"))

(define FRAME-IDLE-L (overlay/align/offset "center" "bottom" RAW-IDLE 0 0 CAJA-REF))
(define FRAME-IDLE-R (flip-horizontal FRAME-IDLE-L))

(define FRAME-BREATHE-L (overlay/align/offset "center" "bottom" RAW-BREATHE 0 0 CAJA-REF))
(define FRAME-BREATHE-R (flip-horizontal FRAME-BREATHE-L))

(define FRAME-WALK-L (overlay/align/offset "center" "bottom" RAW-WALK 0 -2 CAJA-REF))
(define FRAME-WALK-R (flip-horizontal FRAME-WALK-L))

; CONTROLADOR DE ANIMACIÓN
(define (animar-fantasma tick vx dir)
  (if (= vx 0)
      (let ((fase (modulo (quotient tick 15) 2))) 
        (if (= dir 1)
            (if (= fase 0) FRAME-IDLE-R FRAME-BREATHE-R)
            (if (= fase 0) FRAME-IDLE-L FRAME-BREATHE-L)))
      
      (let ((paso (modulo (quotient tick 3) 2))) 
        (if (= dir 1)
            (if (= paso 0) FRAME-IDLE-R FRAME-WALK-R)
            (if (= paso 0) FRAME-IDLE-L FRAME-WALK-L)))))


; ==============================================================================
;                                 SECCIÓN 4
;                         ELEMENTOS DEL ESCENARIO
; ==============================================================================

(define PUERTA-GRID 
  (list
   '(0 1 1 1 1 1 1 0)
   '(1 1 1 1 1 1 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)
   '(1 1 2 2 2 2 1 1)))
(define PUERTA-IMG (render-pixel-art PUERTA-GRID 6 "white" "black")) 

; FLECHA INDICADORA
(define FLECHA-GRID 
  (list
   '(0 0 1 1 1 0 0)
   '(0 0 1 1 1 0 0)
   '(0 0 1 1 1 0 0)
   '(0 0 1 1 1 0 0)
   '(1 1 1 1 1 1 1)
   '(0 1 1 1 1 1 0)
   '(0 0 1 1 1 0 0)
   '(0 0 0 1 0 0 0)))
(define FLECHA-IMG (render-pixel-art FLECHA-GRID 4 "white" "white"))

; ICONO HOME (MENU)
(define CASA-GRID 
  (list
   '(0 0 0 1 1 0 0 0)
   '(0 0 1 1 1 1 0 0)
   '(0 1 1 1 1 1 1 0)
   '(1 1 1 1 1 1 1 1)
   '(1 1 0 0 0 0 1 1)
   '(1 1 0 1 1 0 1 1)
   '(1 1 0 1 1 0 1 1)
   '(1 1 1 1 1 1 1 1)))
(define CASA-IMG (render-pixel-art CASA-GRID 4 "white" "black"))

; CANDADO (NIVEL BLOQUEADO)
(define CANDADO-GRID 
  (list
   '(0 0 1 1 1 0 0)
   '(0 1 0 0 0 1 0)
   '(0 1 0 0 0 1 0)
   '(1 1 1 1 1 1 1)
   '(1 2 1 1 1 2 1)
   '(1 1 1 1 1 1 1)
   '(1 1 1 1 1 1 1)))
(define CANDADO-IMG (render-pixel-art CANDADO-GRID 5 "gray" "red"))

; SUELO EXTENDIDO (Necesario para que cubra los 1366 pixeles)
(define CUADRO-SUELO (overlay (square 40 "outline" "white") (square 40 "solid" "black")))
(define PISO-IMG (apply beside (make-list 35 CUADRO-SUELO))) 
(define COLUMNA-IMG (above CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO CUADRO-SUELO))
(define PINCHO (triangle 20 "solid" "red"))

; GENERADOR DE ESTRELLAS RECURSIVO
(define (crear-estrellas n img)
  (if (= n 0) img
      (place-image (circle 1 "solid" "gray") 
                   (random ANCHO) (random ALTO) 
                   (crear-estrellas (- n 1) img))))

(define FONDO-ESTRELLADO (crear-estrellas 120 (empty-scene ANCHO ALTO "black")))


;                                 SECCIÓN 5
;                          SISTEMA DE FUENTES

(define (dibujar-letra-peq matriz tam color)
  (define (fila->img f) (apply beside (map (lambda (b) (if (= b 1) (square tam "solid" color) (square tam "solid" "transparent"))) f)))
  (apply above (map fila->img matriz)))

; Definicion detallada de cada carácter para el sistema de texto propio.

(define S-MAT 
  (list 
   '(0 1 1 1 1) 
   '(1 0 0 0 0) 
   '(1 1 1 1 1) 
   '(0 0 0 0 1) 
   '(1 1 1 1 0)))

(define A-MAT 
  (list 
   '(0 1 1 1 0) 
   '(1 0 0 0 1) 
   '(1 1 1 1 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1)))

(define V-MAT 
  (list 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(0 1 0 1 0) 
   '(0 0 1 0 0)))

(define E-MAT 
  (list 
   '(1 1 1 1 1) 
   '(1 0 0 0 0) 
   '(1 1 1 1 0) 
   '(1 0 0 0 0) 
   '(1 1 1 1 1)))

(define U-MAT 
  (list 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(0 1 1 1 0)))

(define T-MAT 
  (list 
   '(1 1 1 1 1) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0)))

(define R-MAT 
  (list 
   '(1 1 1 1 0) 
   '(1 0 0 0 1) 
   '(1 1 1 1 0) 
   '(1 0 1 0 0) 
   '(1 0 0 1 0)))

(define L-MAT 
  (list 
   '(1 0 0 0 0) 
   '(1 0 0 0 0) 
   '(1 0 0 0 0) 
   '(1 0 0 0 0) 
   '(1 1 1 1 1)))

(define C-MAT 
  (list 
   '(0 1 1 1 1) 
   '(1 0 0 0 0) 
   '(1 0 0 0 0) 
   '(1 0 0 0 0) 
   '(0 1 1 1 1)))

(define G-MAT 
  (list 
   '(0 1 1 1 1) 
   '(1 0 0 0 0) 
   '(1 0 1 1 1) 
   '(1 0 0 0 1) 
   '(0 1 1 1 1)))

(define M-MAT 
  (list 
   '(1 0 0 0 1) 
   '(1 1 0 1 1) 
   '(1 0 1 0 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1)))

(define O-MAT 
  (list 
   '(0 1 1 1 0) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(1 0 0 0 1) 
   '(0 1 1 1 0)))

(define N-MAT 
  (list 
   '(1 0 0 0 1) 
   '(1 1 0 0 1) 
   '(1 0 1 0 1) 
   '(1 0 0 1 1) 
   '(1 0 0 0 1)))

(define Y-MAT 
  (list 
   '(1 0 0 0 1) 
   '(0 1 0 1 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0)))

(define I-MAT 
  (list 
   '(0 1 1 1 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 1 1 1 0)))

(define N1-MAT 
  (list 
   '(0 0 1 0 0) 
   '(0 1 1 0 0) 
   '(0 0 1 0 0) 
   '(0 0 1 0 0) 
   '(0 1 1 1 0)))

(define N2-MAT 
  (list 
   '(0 1 1 1 0) 
   '(1 0 0 0 1) 
   '(0 0 0 1 0) 
   '(0 0 1 0 0) 
   '(1 1 1 1 1)))

(define N3-MAT 
  (list 
   '(1 1 1 1 0) 
   '(0 0 0 0 1) 
   '(0 1 1 1 0) 
   '(0 0 0 0 1) 
   '(1 1 1 1 0)))

(define DOS-PUNTOS-MAT 
  (list 
   '(0 0 0 0 0) 
   '(0 0 1 0 0) 
   '(0 0 0 0 0) 
   '(0 0 1 0 0) 
   '(0 0 0 0 0)))

; CONSTRUCTOR DE CADENAS DE TEXTO 
(define (pixel-text str tam color)
  (define (char->img c)
    (cond [(char=? c #\S) (dibujar-letra-peq S-MAT tam color)]
          [(char=? c #\A) (dibujar-letra-peq A-MAT tam color)]
          [(char=? c #\V) (dibujar-letra-peq V-MAT tam color)]
          [(char=? c #\E) (dibujar-letra-peq E-MAT tam color)]
          [(char=? c #\U) (dibujar-letra-peq U-MAT tam color)]
          [(char=? c #\T) (dibujar-letra-peq T-MAT tam color)]
          [(char=? c #\R) (dibujar-letra-peq R-MAT tam color)]
          [(char=? c #\L) (dibujar-letra-peq L-MAT tam color)]
          [(char=? c #\C) (dibujar-letra-peq C-MAT tam color)]
          [(char=? c #\G) (dibujar-letra-peq G-MAT tam color)]
          [(char=? c #\M) (dibujar-letra-peq M-MAT tam color)]
          [(char=? c #\O) (dibujar-letra-peq O-MAT tam color)]
          [(char=? c #\N) (dibujar-letra-peq N-MAT tam color)]
          [(char=? c #\Y) (dibujar-letra-peq Y-MAT tam color)]
          [(char=? c #\I) (dibujar-letra-peq I-MAT tam color)]
          [(char=? c #\1) (dibujar-letra-peq N1-MAT tam color)]
          [(char=? c #\2) (dibujar-letra-peq N2-MAT tam color)]
          [(char=? c #\3) (dibujar-letra-peq N3-MAT tam color)]
          [(char=? c #\:) (dibujar-letra-peq DOS-PUNTOS-MAT tam color)]
          [else (square tam "solid" "transparent")]))
  
  (let ((lista-imagenes (map (lambda (c) (beside (char->img c) (square (/ tam 2) "solid" "transparent"))) (string->list str))))
    (if (= (length lista-imagenes) 1) (first lista-imagenes) (apply beside lista-imagenes))))

; CACHÉ DE ETIQUETAS (textos estáticos) 

(define TXT-NIVEL-LABEL (pixel-text "NIVEL: " 6 "white"))
(define TXT-SELECT (pixel-text "SELECT LEVEL" 8 "white"))
(define TXT-GANASTE (pixel-text "GANASTE" 10 "green"))
(define TXT-GAMEOVER (pixel-text "GAME OVER" 10 "red"))
(define TXT-START-W (pixel-text "START" 5 "white"))
(define TXT-START-B (pixel-text "START" 5 "black"))
(define TXT-RETRY (pixel-text "RETRY" 5 "white"))

; ANIMACIÓN DEL TÍTULO 
(define TAM-PIXEL-TITULO 15)
(define (matriz->puntos-simple matriz offset-x tam)
  (for*/list ([row-idx (in-range (length matriz))] [col-idx (in-range (length (first matriz)))]
              #:when (= (list-ref (list-ref matriz row-idx) col-idx) 1))
    (list (+ offset-x (* col-idx tam)) (* row-idx tam))))

(define PUNTOS-TITULO 
  (reverse (append (matriz->puntos-simple S-MAT 0 TAM-PIXEL-TITULO) 
                   (matriz->puntos-simple A-MAT (* 6 TAM-PIXEL-TITULO) TAM-PIXEL-TITULO) 
                   (matriz->puntos-simple V-MAT (* 12 TAM-PIXEL-TITULO) TAM-PIXEL-TITULO) 
                   (matriz->puntos-simple E-MAT (* 18 TAM-PIXEL-TITULO) TAM-PIXEL-TITULO) 
                   (matriz->puntos-simple U-MAT (* 26 TAM-PIXEL-TITULO) TAM-PIXEL-TITULO) 
                   (matriz->puntos-simple S-MAT (* 32 TAM-PIXEL-TITULO) TAM-PIXEL-TITULO))))
(define TOTAL-PIXELES (length PUNTOS-TITULO))

(define CENTRO-X-PANTALLA (/ ANCHO 2))

(define (dibujar-titulo-animado n imagen-base x-centro y-centro)
  (let* ((puntos (take PUNTOS-TITULO (min n TOTAL-PIXELES))) 
         (bloque (square TAM-PIXEL-TITULO "solid" "white")) 
         (sx (- x-centro 280)) (sy (- y-centro 50)))
    (foldl (lambda (pt img) (place-image bloque (+ sx (first pt)) (+ sy (second pt)) img)) imagen-base puntos)))


;                                 SECCIÓN 6
;                       INTERFAZ DE USUARIO (GUI)

;  ICONOS DEL HUD 
(define ICO-PAUSE 
  (render-pixel-art 
   (list '(1 1 0 1 1) '(1 1 0 1 1) '(1 1 0 1 1) '(1 1 0 1 1) '(1 1 0 1 1)) 
   4 "white" "black"))

(define ICO-PLAY (triangle 15 "solid" "white")) 

(define ICO-MUSIC 
  (render-pixel-art 
   (list '(0 0 0 1 1 1) '(0 0 0 1 0 1) '(0 0 0 1 0 1) '(0 0 0 1 0 1) '(1 1 1 1 0 1) '(1 1 1 1 0 0) '(1 1 1 1 0 0)) 
   4 "white" "black"))

(define ICO-MUSIC-OFF 
  (render-pixel-art 
   (list '(1 0 0 1 1 1) '(0 1 0 1 0 1) '(0 0 1 1 0 1) '(0 0 0 1 0 1) '(1 1 1 1 1 1) '(1 1 1 1 0 0) '(1 1 1 1 0 0)) 
   4 "gray" "black"))

(define ICO-HOME-S 
  (render-pixel-art 
   (list '(0 0 1 0 0) '(0 1 1 1 0) '(1 1 1 1 1) '(1 0 1 0 1) '(1 0 1 0 1)) 
   4 "white" "black"))

(define ICO-ARROW-BTN 
  (render-pixel-art 
   (list '(0 0 0 0 1 0 0) '(0 0 0 0 1 1 0) '(1 1 1 1 1 1 1) '(0 0 0 0 1 1 0) '(0 0 0 0 1 0 0)) 
   6 "white" "black"))

;  CONSTRUCTORES DE BOTONES 
(define (crear-marco ancho alto color-borde grosor)
  (overlay (rectangle ancho alto "solid" "black") 
           (rectangle (+ ancho grosor) (+ alto grosor) "outline" (pen color-borde grosor "solid" "butt" "bevel"))))

(define BTN-START-FRAME (crear-marco 190 50 "white" 4))
(define (get-btn-start blink?) (overlay (if blink? TXT-START-W TXT-START-B) BTN-START-FRAME))
(define BTN-RETRY-IMG (overlay TXT-RETRY BTN-START-FRAME))

(define FRAME-NIVEL-WHITE (crear-marco 60 60 "white" 4))
(define FRAME-NIVEL-GRAY (crear-marco 60 60 "gray" 4))

(define (btn-nivel n max-n tick)
  (cond [(> n max-n) (overlay CANDADO-IMG FRAME-NIVEL-GRAY)]
        [(= n max-n) (overlay (pixel-text (number->string n) 8 "white") (if (even? (quotient tick 15)) FRAME-NIVEL-WHITE FRAME-NIVEL-GRAY))]
        [else (overlay (pixel-text (number->string n) 8 "white") FRAME-NIVEL-WHITE)]))

(define BTN-HOME-IMG (overlay CASA-IMG FRAME-NIVEL-WHITE))
(define BTN-NEXT-IMG (overlay ICO-ARROW-BTN FRAME-NIVEL-WHITE))
(define FRAME-HUD (crear-marco 35 35 "white" 3))
(define (crear-btn-hud icono) (overlay icono FRAME-HUD))

;                                 SECCIÓN 7
;                          SISTEMA DE PARTÍCULAS

; Estructura de partícula (chispa)
(struct particula (x y vx vy vida color) #:transparent)

; Crea una explosión de N partículas rojas
(define (crear-explosion x y n color)
  (for/list ([i (in-range n)])
    (particula x y 
               (- (random 14) 7) 
               (- (random 14) 7) 
               15 color)))

; Actualiza la física de las partículas y elimina las viejas
(define (actualizar-particulas lista)
  (filter (lambda (p) (> (particula-vida p) 0))
          (map (lambda (p) (struct-copy particula p 
                                        [x (+ (particula-x p) (particula-vx p))]
                                        [y (+ (particula-y p) (particula-vy p))]
                                        [vida (- (particula-vida p) 1)])) lista)))


;                                 SECCIÓN 8
;                          LÓGICA DEL JUEGO

; Estructura principal del estado del mundo
(struct juego (escena nivel max-nivel x y vx vy fantasmas tick dir pausa? musica? particulas) #:transparent)

; Estado Inicial
(define INICIO (juego "menu" 1 1 -50 PISO-Y 8 0 0 0 1 #f #t '()))

; DIFICULTAD DE NIVELES (ADAPTADA A 1366 PIXELES)
(define (obtener-obstaculos nivel tick)
  (cond [(= nivel 1) (list (list 500 PISO-Y) (list 900 PISO-Y))]
        [(= nivel 2) (list (list 400 PISO-Y) (list 800 PISO-Y) (list 1200 PISO-Y))]
        [(= nivel 3) (list (list 350 PISO-Y) (list 600 PISO-Y) (list 850 PISO-Y) (list 1100 PISO-Y))]
        [else '()]))

; COLISIONES
(define (choca? px py ox oy) (and (> px (- ox 20)) (< px (+ ox 20)) (> py (- oy 30))))

(define (colision-grupo? x y num-fantasmas obs-list)
  (for/or ([i (in-range (+ num-fantasmas 1))])
    (let ((fx (- x (* i 70)))) (ormap (lambda (o) (choca? fx y (first o) (second o))) obs-list))))


(define (actualizar m)
  (let* ((escena (juego-escena m)) (x (juego-x m)) (y (juego-y m)) (vy (juego-vy m)) (vx (juego-vx m))
         (nivel (juego-nivel m)) (tick (juego-tick m)) (pausa (juego-pausa? m))
         ; Procesamiento de partículas en cada frame
         (parts (actualizar-particulas (juego-particulas m))))
    
    (cond
      ; PAUSA
      [(and (string=? escena "juego") pausa) m]
      
      ; MENÚ Y SELECCIÓN
      [(string=? escena "menu") (struct-copy juego m [tick (+ tick VELOCIDAD-TITULO)] [particulas parts])]
      [(string=? escena "seleccion") (struct-copy juego m [tick (+ tick 1)] [particulas parts])]
      
      ; JUEGO
      [(string=? escena "juego")
       (let* ((nuevo-x (max 0 (+ x vx))) 
              (nuevo-vy (+ vy GRAVEDAD)) 
              (nuevo-y (+ y nuevo-vy))
              (final-py (if (>= nuevo-y PISO-Y) PISO-Y nuevo-y)) 
              (final-vy (if (>= nuevo-y PISO-Y) 0 nuevo-vy))
              (obs (obtener-obstaculos nivel tick))
              (num-seguidores (if (= nivel 1) 1 (if (= nivel 2) 3 5)))
              (posicion-ultimo (- x (* num-seguidores 70))))
         
         (cond 
           ; GANAR (Sin chispas)
           [(> posicion-ultimo META-X) 
            (struct-copy juego m 
                         [escena "ganaste"] 
                         [particulas '()] 
                         [max-nivel (max (juego-max-nivel m) (if (< nivel MAX-NIVELES) (add1 nivel) nivel))])]
           
           ; PERDER (Con explosión roja)
           [(colision-grupo? x final-py num-seguidores obs) 
            (struct-copy juego m 
                         [escena "perdiste"]
                         [particulas (crear-explosion x final-py 30 "red")])] 
           
           ;MOVERSE
           [else (struct-copy juego m [x nuevo-x] [y final-py] [vy final-vy] [tick (+ tick 1)] [particulas parts])]))]
      
      ; PANTALLAS FINALES
      [else (struct-copy juego m [particulas parts])])))

;                                 SECCIÓN 9
;                          RENDERIZADO (DIBUJO)

(define (dibujar-tren x y n img tick vx dir)
  (if (< n 0) img
      (let ((img-actual (animar-fantasma tick vx dir)))
        (place-image img-actual x (- y 20) (dibujar-tren (- x 70) y (- n 1) img tick vx dir)))))

(define (dibujar-obs lista img) (foldl (lambda (o im) (place-image PINCHO (first o) (- (second o) 10) im)) img lista))
(define (dibujar-particulas lista img) (foldl (lambda (p im) (place-image (square 4 "solid" (particula-color p)) (particula-x p) (particula-y p) im)) img lista))

(define (dibujar-hud m img-base)
  (let* ((num-str (number->string (juego-nivel m)))
         (pausa? (juego-pausa? m))
         (musica? (juego-musica? m))
         (btn-pausa (crear-btn-hud (if pausa? ICO-PLAY ICO-PAUSE)))
         (btn-musica (crear-btn-hud (if musica? ICO-MUSIC ICO-MUSIC-OFF)))
         (btn-home (crear-btn-hud ICO-HOME-S))
         (hud-x-center CENTRO-X-PANTALLA)
         (hud-x-buttons (- ANCHO 100))) 
    (place-image (beside TXT-NIVEL-LABEL (pixel-text num-str 6 "white")) hud-x-center 30
                 (place-image btn-pausa (- hud-x-buttons 100) 35 
                              (place-image btn-musica (- hud-x-buttons 50) 35 
                                           (place-image btn-home hud-x-buttons 35 img-base))))))

(define (renderizar m)
  (define escena (juego-escena m))
  
  ; SELECCIÓN DE FONDO: ESTRELLAS SOLO EN JUEGO
  (define fondo-base 
    (if (string=? escena "juego") 
        FONDO-ESTRELLADO 
        (empty-scene ANCHO ALTO COLOR-FONDO)))
  
  ; AGREGAR PARTÍCULAS SOBRE EL FONDO
  (define base (dibujar-particulas (juego-particulas m) fondo-base))
  
  (cond
    ; ESCENA MENU
    [(string=? escena "menu")
     (let* ((tick (juego-tick m)) (fondo-con-titulo (dibujar-titulo-animado tick base CENTRO-X-PANTALLA 150)))
       (if (> tick TOTAL-PIXELES) (place-image (get-btn-start (< (modulo (- tick TOTAL-PIXELES) 60) 40)) CENTRO-X-PANTALLA 300 fondo-con-titulo) fondo-con-titulo))]
    
    ; ESCENA SELECCION
    [(string=? escena "seleccion")
     (let* ((max-n (juego-max-nivel m)) (tick (juego-tick m))
            (c CENTRO-X-PANTALLA))
       (place-image TXT-SELECT c 80
                    (place-image (btn-nivel 1 max-n tick) (- c 283) 200
                                 (place-image (btn-nivel 2 max-n tick) c 200
                                              (place-image (btn-nivel 3 max-n tick) (+ c 283) 200 
                                                           (place-image BTN-HOME-IMG c 320 base))))))]
    
    ; ESCENA JUEGO
    [(string=? escena "juego")
     (let* ((nivel (juego-nivel m)) (num-f (if (= nivel 1) 1 (if (= nivel 2) 3 5))) (obs (obtener-obstaculos nivel (juego-tick m)))
            (tick (juego-tick m)) (flecha-y (- PISO-Y 70 (* 5 (sin (/ tick 5)))))
            (mundo (place-image COLUMNA-IMG 20 120 (place-image COLUMNA-IMG (- ANCHO 20) 120 
                    (place-image FLECHA-IMG META-X flecha-y (place-image PUERTA-IMG META-X (- PISO-Y 30)
                     (dibujar-obs obs (dibujar-tren (juego-x m) (juego-y m) num-f (place-image PISO-IMG (/ ANCHO 2) (+ PISO-Y 20) base) tick (juego-vx m) (juego-dir m)))))))))
       (dibujar-hud m mundo))]
    
    ; ESCENA GANASTE
    [(string=? escena "ganaste")
     (let* ((nivel-actual (juego-nivel m)) (hay-siguiente? (< nivel-actual MAX-NIVELES)) (base-msg (place-image TXT-GANASTE CENTRO-X-PANTALLA 150 base)))
       (if hay-siguiente? 
           (place-image BTN-NEXT-IMG (+ CENTRO-X-PANTALLA 50) 280 (place-image BTN-HOME-IMG (- CENTRO-X-PANTALLA 50) 280 base-msg)) 
           (place-image BTN-HOME-IMG CENTRO-X-PANTALLA 280 base-msg)))]
    
    ; ESCENA PERDISTE
    [(string=? escena "perdiste") 
     (place-image TXT-GAMEOVER CENTRO-X-PANTALLA 150 (place-image BTN-RETRY-IMG CENTRO-X-PANTALLA 260 (place-image BTN-HOME-IMG CENTRO-X-PANTALLA 340 base)))]))


;                                 SECCIÓN 10
;                           ENTRADA (INPUT)

(define (teclado m k)
  (cond [(string=? (juego-escena m) "juego")
         (cond [(juego-pausa? m) m]
               [(and (string=? k "w") (>= (juego-y m) PISO-Y)) (struct-copy juego m [vy SALTO])]
               [(string=? k "d") (struct-copy juego m [vx VELOCIDAD] [dir 1])] 
               [(string=? k "a") (struct-copy juego m [vx (- VELOCIDAD)] [dir -1])]
               [else m])] [else m]))

(define (soltar m k)
  (if (string=? (juego-escena m) "juego") (cond [(or (string=? k "a") (string=? k "d")) (struct-copy juego m [vx 0])] [else m]) m))

(define (mouse m x y e)
  (if (string=? e "button-down")
      (cond
        [(string=? (juego-escena m) "menu")
         (if (and (> (juego-tick m) TOTAL-PIXELES) 
                  (> x (- CENTRO-X-PANTALLA 100)) (< x (+ CENTRO-X-PANTALLA 100)) 
                  (> y 270) (< y 330))
             (struct-copy juego m [escena "seleccion"] [x -50] [fantasmas 0] [tick 0]) m)]
        [(string=? (juego-escena m) "seleccion")
         (let ((c CENTRO-X-PANTALLA))
           (cond [(and (> x (- (- c 283) 30)) (< x (+ (- c 283) 30)) (> y 165) (< y 235)) (iniciar-nivel m 1)]
                 [(and (> x (- c 30)) (< x (+ c 30)) (> y 165) (< y 235) (>= (juego-max-nivel m) 2)) (iniciar-nivel m 2)]
                 [(and (> x (- (+ c 283) 30)) (< x (+ (+ c 283) 30)) (> y 165) (< y 235) (>= (juego-max-nivel m) 3)) (iniciar-nivel m 3)]
                 [(and (> x (- c 30)) (< x (+ c 30)) (> y 290) (< y 350)) (struct-copy juego m [escena "menu"] [tick 0] [x -50])] 
                 [else m]))]
        [(string=? (juego-escena m) "juego")
         (let ((hx (- ANCHO 100)))
           (cond [(and (> x (- hx 115)) (< x (- hx 85)) (> y 15) (< y 55)) (struct-copy juego m [pausa? (not (juego-pausa? m))])]
                 [(and (> x (- hx 65)) (< x (- hx 35)) (> y 15) (< y 55)) (struct-copy juego m [musica? (not (juego-musica? m))])]
                 [(and (> x (- hx 15)) (< x (+ hx 15)) (> y 15) (< y 55)) (struct-copy juego m [escena "menu"] [x -50] [tick 0])] 
                 [else m]))]
        [(string=? (juego-escena m) "ganaste")
         (let ((hay-siguiente? (< (juego-nivel m) MAX-NIVELES)) (c CENTRO-X-PANTALLA))
           (cond [(if hay-siguiente? 
                      (and (> x (- (- c 50) 30)) (< x (+ (- c 50) 30)) (> y 250) (< y 310)) 
                      (and (> x (- c 30)) (< x (+ c 30)) (> y 250) (< y 310))) 
                  (struct-copy juego m [escena "menu"] [tick 0] [x -50])]
                 [(and hay-siguiente? (> x (- (+ c 50) 30)) (< x (+ (+ c 50) 30)) (> y 250) (< y 310)) (iniciar-nivel m (add1 (juego-nivel m)))] 
                 [else m]))]
        [(string=? (juego-escena m) "perdiste")
         (let ((c CENTRO-X-PANTALLA))
           (cond [(and (> x (- c 95)) (< x (+ c 95)) (> y 235) (< y 285)) (iniciar-nivel m (juego-nivel m))]
                 [(and (> x (- c 30)) (< x (+ c 30)) (> y 310) (< y 370)) (struct-copy juego m [escena "menu"] [tick 0] [x -50])] 
                 [else m]))]
        [else m]) 
      m))

(define (iniciar-nivel m n)
  (struct-copy juego m [escena "juego"] [nivel n] [x 100] [y PISO-Y] [vx 0] [vy 0] [tick 0] [dir 1] [pausa? #f] [particulas '()]))
;                                 SECCIÓN 11
;                             INICIO DEL JUEGO

(big-bang INICIO 
  (to-draw renderizar) 
  (on-tick actualizar 1/30) 
  (on-key teclado) 
  (on-release soltar) 
  (on-mouse mouse) 
  (name "SAVE US: FINAL EDITION"))
