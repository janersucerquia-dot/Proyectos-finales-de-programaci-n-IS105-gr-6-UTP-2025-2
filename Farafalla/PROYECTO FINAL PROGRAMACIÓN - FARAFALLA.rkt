#lang racket

;; ==========================
;; LIBRERÍAS
;; ==========================

; Carga librerías de gráficos para dibujar en pantalla
(require graphics/graphics)

; Permite manejar ventanas, eventos, sonidos y GUI
(require racket/gui)

; Permite cargar archivos (imágenes, sonidos, etc.) sin que falle la ruta
(require racket/runtime-path)



;; ==========================
;; VENTANA PRINCIPAL
;; ==========================

; Inicializa el sistema de gráficos de Racket
(open-graphics)

; Define el ancho de la ventana principal
(define WIDTH  900)

; Define el alto de la ventana principal
(define HEIGHT 800)

; Crea la ventana del juego con el título FARAFALLA
; vent es la variable que representa la ventana
(define vent (open-viewport "FARAFALLA" WIDTH HEIGHT))



;; ==========================
;; SONIDO CINEMÁTICA
;; ==========================

; Define una ruta hacia el archivo de sonido de la cinemática
; runtime-path evita errores al mover el proyecto de carpeta
(define-runtime-path sonido
  "sonidos proyecto/cinematica/cinematica.wav")

; Función que reproduce el sonido de la cinemática
; #t = reproduce sin detener la ejecución del programa
(define (sonido-cinematica)
  (play-sound sonido #t))



;; ===========================
;; CONSTANTES DEL JUEGO Y DEL MUNDO
;; ===========================

; Tamaño Completo del mundo
(define NIVEL-ANCHO 90000)
(define NIVEL-ALTO 20000)

; Tamaño del bicho
(define BICHO-ANCHO 40)
(define BICHO-ALTO  78)

; La velocidad con la que cae 
(define GRAVEDAD 2)

; La velocidad con la que salta
(define IMPULSO-SALTO -30)

; Los cuadros que camina al moverlo de las flechas
(define PASO 14)

; La velocidad de cada frame
(define TIEMPO 0.02)

; La posición del bicho al iniciar
(define INICIO 21500)

;; Hacia dónde mira el bichito ('left o 'right)
(define current-direction 'right)

;; Paso actual de la animación (0, 1, 2, ...)
(define anim-step 0)

(define MARGEN-X 6)   ; Margen horizontal (3 píxeles por cada lado)
(define MARGEN-Y 2)   ; Margen vertical (3 píxeles por arriba y abajo)



;; ==================================
;; INVENTARIO DE GLOBOS DEL JUGADOR
;; ==================================

;; Lista de colores de globos que ya tiene el bicho, ej: '(naranja rosa)
(define globos-obtenidos '())

(define (tiene-globo? color)
  (member color globos-obtenidos))

(define (agregar-globo! color)
  (when (not (tiene-globo? color))
    (set! globos-obtenidos (cons color globos-obtenidos))))

;; Si quieres que el globo se "gaste" al abrir la puerta:
(define (usar-globo! color)
  (set! globos-obtenidos (remove color globos-obtenidos)))



;; ==========================
;; PLATAFORMAS (LABERINTO)
;; ==========================

(struct plataforma (x y ancho alto))

;; Ajusta estas plataformas para ir armando el laberinto
(define plataformas
  (list
   ;; Piso de la izquierda inicio
   (plataforma 18350 800 2000 580)
   
   ;; Piso de la derecha inicio
   (plataforma 20700 800 1300 570)
   
   ;; Cuadritos iniciales
   (plataforma 20630 890 70 70)
   (plataforma 20350 1100 70 70)
   (plataforma 20630 1300 70 70)
   
   ;; Piso 2da capa
   (plataforma 19500 1500 21000 600)
   (plataforma 17890 1500 1500 600)

   ;;Pared derecha bajo cuadritos iniciales
   (plataforma 21000 1200 450 300)

   ;; Pared 1er globo
   (plataforma 17800 1200 990 700)

   ;; Cuadritos segunda capa
   (plataforma 19450 1650 50 50)
   (plataforma 19380 1790 50 50)
   (plataforma 19450 1960 50 50)
   (plataforma 19230 2100 200 50)

   ;; Piso 3ra capa
   (plataforma 18600 2300 1500 600)
   (plataforma 17600 2100 700 1400)
   (plataforma 20000 2500 600 1200)

   ;;Cuadritos tercera capa
   (plataforma 18250 2650 150 50)
   (plataforma 18500 2450 150 50)
   (plataforma 18500 2850 150 50)
   (plataforma 17700 3000 700 50)

   ;;Piso 4ta capa
   (plataforma 17950 3050 2200 400)

   ;;Pared 2do globo
   (plataforma 19800 2900 600 400)

   ;;Pared tramo final
   (plataforma 20800 2500 2600 1200)

   ;; Cuadritos tramo final
   (plataforma 20700 2700 100 60)
   (plataforma 20600 2900 100 60)
   (plataforma 20700 3100 100 60)
   (plataforma 20600 3300 100 60)
   (plataforma 20700 3500 100 60)

   ;; Piso tramo final
   (plataforma 20000 3400 600 800)
   (plataforma 20000 3800 4500 500)
   (plataforma 24750 3800 4500 500)
   

   ;; Techo tramo final
   (plataforma 23100 2450 2700 700)

   ;; Cuadritos tramo final
   (plataforma 23700 3700 80 350)
   (plataforma 23780 3620 80 700)
   (plataforma 23860 3540 80 1000)
   (plataforma 23860 3480 820 80)
   (plataforma 24900 2800 900 1500)))


;; ==========================
;; GLOBOS Y PUERTAS
;; ==========================

(struct globo (x y ancho alto color activo? imagen) #:mutable)
;; Ahora cada globo guarda SU PROPIA imagen
(struct puerta (x y ancho alto color abierta?) #:mutable)

(define globos
  (list
   ;; Globo naranja con su imagen
   (globo 18830 1390 10 10 'naranja #t "imagenes proyecto/Globos/globo naranja.bmp")
   ;; Globo rosa con su imagen  
   (globo 19670 2930 10 10 'rosa #t "imagenes proyecto/Globos/globo rosado.bmp")))
   ;; Puedes añadir más globos así:
   ;; (globo 20000 1500 50 60 'azul #t "imagenes proyecto/Globos/globo azul.bmp")

;; Lista de puertas en el mundo (AJUSTA coordenadas, tamaños y colores)
(define puertas
  (list
   ;; Puerta naranja, bloqueando un paso
   (puerta 18700 2100 80 200 'naranja #f)
   ;; Puerta rosa en otro punto
   (puerta 19700 2100 80 200 'rosa #f)))



;; ==========================
;; CONTROLES DEL TECLADO
;; ==========================

; Current-h-key: Función que lee el teclado y guarda la tecla que se toque
(define current-h-key 'none)

; Solo para saltar: muestra #t cuando esa tecla se presiona
; La pone en #f cuando la tecla anteriormente ya estuvo en #t
(define jump-request? #f)

(define (teclado-loop)
  (let* ([evt (get-key-press vent)]
         [k   (key-value evt)])
    (cond
      [(eq? k 'left)
       (set! current-h-key 'left)
       (set! current-direction 'left)]   ; mira a la izquierda

      [(eq? k 'right)
       (set! current-h-key 'right)
       (set! current-direction 'right)]  ; mira a la derecha

      [(eq? k 'up)
       (set! jump-request? #t)]

      [else
       (set! current-h-key 'none)])
    (teclado-loop)))

(thread teclado-loop)



;; =============================================
;; SELECCIONAR SPRITE SEGÚN ANIMACIÓN Y DIRECCIÓN
;; =============================================
(define (bicho-sprite-path)
  (define caminando? (not (eq? current-h-key 'none)))

  (cond
    ;; ---------------------------
    ;; QUIETO
    ;; ---------------------------
    [(not caminando?)
     (if (eq? current-direction 'right)
         "imagenes proyecto/Buhito/bicho parado derecha.bmp"
         "imagenes proyecto/Buhito/bicho parado izquierda.bmp")]

    ;; ---------------------------
    ;; CAMINANDO A LA DERECHA
    ;; ---------------------------
    [(eq? current-direction 'right)
     (cond
       [(= anim-step 0) "imagenes proyecto/Buhito/bicho der pierna izq.bmp"]
       [(= anim-step 1) "imagenes proyecto/Buhito/bicho parado derecha.bmp"]
       [(= anim-step 2) "imagenes proyecto/Buhito/bicho der todo.bmp"])]
       
    ;; ---------------------------
    ;; CAMINANDO A LA IZQUIERDA
    ;; ---------------------------
    [else
     (cond
       [(= anim-step 0) "imagenes proyecto/Buhito/bicho izq pierna der.bmp"]
       [(= anim-step 1) "imagenes proyecto/Buhito/bicho parado izquierda.bmp"]
       [(= anim-step 2) "imagenes proyecto/Buhito/bicho izq todo.bmp"])]))



;; ==========================
;; CÁMARA QUE SIGUE AL JUGADOR
;; ==========================

(define (camera-x-from-player x)
  (let* ([medio (/ WIDTH 2)]
         [raw   (- x medio)])              ; intenta centrar al bicho
    (max 0 (min raw (- NIVEL-ANCHO WIDTH))))
  ) ; no se sale del nivel

(define (camera-y-from-player y)
  (let* ([medio (/ HEIGHT 2)]
         [raw   (- y medio)])              ; intenta centrar al bicho en vertical
    (max 0 (min raw (- NIVEL-ALTO HEIGHT))))
  ) ; no se sale del nivel



;; ==========================
;; FUNCIONES DE COLISIÓN
;; ==========================

;; Colisión genérica del bicho con un rectángulo cualquiera
(define (colision-bicho-con-rect? bx by rx ry ran ral)
  ;; Coordenadas del sprite completo
  (define sprite-left (- bx (/ BICHO-ANCHO 2)))
  (define sprite-top  (- by BICHO-ALTO))

  ;; Hitbox recortada (márgenes)
  (define bicho-left   (+ sprite-left MARGEN-X))
  (define bicho-top    (+ sprite-top MARGEN-Y))
  (define bicho-right  (- (+ sprite-left BICHO-ANCHO) MARGEN-X))
  (define bicho-bottom (- (+ sprite-top  BICHO-ALTO)  MARGEN-Y))

  (define rect-left   rx)
  (define rect-top    ry)
  (define rect-right  (+ rx ran))
  (define rect-bottom (+ ry ral))

  (and (< bicho-left rect-right)
       (> bicho-right rect-left)
       (< bicho-top rect-bottom)
       (> bicho-bottom rect-top)))

;; Función específica para plataformas (usa la genérica)
(define (colision-bicho-plataforma? bx by p)
  (colision-bicho-con-rect?
   bx by
   (plataforma-x p)
   (plataforma-y p)
   (plataforma-ancho p)
   (plataforma-alto p)))




;; ==========================
;; DIBUJAR ESCENA CON CÁMARA
;; ==========================

(define (dibujar-escena x y)
  (define cam-x (camera-x-from-player x))
  (define cam-y (camera-y-from-player y))

  (clear-viewport vent)

  ;; Cielo
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.7 1.0))

  

  ;; =========================
  ;; PLATAFORMAS
  ;; =========================
  (for-each
   (λ (p)
     (define px  (plataforma-x p))
     (define py  (plataforma-y p))
     (define pan (plataforma-ancho p))
     (define pal (plataforma-alto p))
     ((draw-solid-rectangle vent)
      (make-posn (- px cam-x) (- py cam-y))
      pan
      pal
      (make-rgb 0.15 0.25 0.15)))
   plataformas)

  ;; =========================
  ;; GLOBOS 
  ;; =========================
  
(for-each
 (λ (g)
   (when (globo-activo? g)
     ;; Dibuja directamente la imagen del globo
     ((draw-pixmap vent)
      (globo-imagen g)                    ; <- USA LA IMAGEN DEL GLOBO
      (make-posn (- (globo-x g) cam-x)
                 (- (globo-y g) cam-y))
      "black")))
 globos)

  ;; =========================
  ;; PUERTAS (solo si NO están abiertas)
  ;; =========================
  (for-each
   (λ (pu)
     (unless (puerta-abierta? pu)
       ;; Si quieres usar imagen, cambia este rectángulo por draw-pixmap
       ((draw-solid-rectangle vent)
        (make-posn (- (puerta-x pu) cam-x)
                   (- (puerta-y pu) cam-y))
        (puerta-ancho pu)
        (puerta-alto pu)
        (cond
          [(eq? (puerta-color pu) 'naranja) (make-rgb 0.9 0.4 0.0)]
          [(eq? (puerta-color pu) 'rosa)    (make-rgb 1.0 0.6 0.8)]
          [else                             (make-rgb 0.6 0.6 0.6)]))))
   puertas)
 
  ;; =========================
  ;; BICHITA (NOVIA) AL FINAL
  ;; =========================
  ((draw-pixmap vent)
   "imagenes proyecto/Buhita/bichita.bmp"
   (make-posn (- 24000 cam-x) (- 3600 cam-y))  ; Ajusta las coordenadas aquí
   "black")

  ;; =========================
  ;; BICHITO
  ;; =========================
  (let* ([x-izq        (- x (/ BICHO-ANCHO 2))]
         [x-izq-screen (- x-izq cam-x)]
         [y-arriba     (- y BICHO-ALTO)]
         [y-arriba-screen (- y-arriba cam-y)])
    ((draw-pixmap vent)
     (bicho-sprite-path)
     (make-posn x-izq-screen y-arriba-screen)
     "black")))



;; ==========================
;; CAÍDA INICIAL SIMPLIFICADA
;; ==========================

(define (caida-inicial)
  (define x INICIO)
  (define y 0)  ; Empezar desde arriba
  (define vy 0)
  
  (let loop ()
    (dibujar-escena x y)
    (sleep TIEMPO)
    
    ;; Aplicar gravedad
    (set! vy (+ vy GRAVEDAD))
    (set! y (+ y vy))
    
    ;; Verificar si toca alguna plataforma
    (define toca-plataforma?
      (for/or ([p plataformas])
        (colision-bicho-plataforma? x y p)))
    
    (cond
      [toca-plataforma?
       ;; Encontrar la plataforma más alta que está tocando
       (define plataforma-mas-alta
         (for/fold ([altura-mas-alta -1]) ([p plataformas])
           (if (and (colision-bicho-plataforma? x y p)
                    (> (plataforma-y p) altura-mas-alta))
               (plataforma-y p)
               altura-mas-alta)))
       
       ;; Colocar al bicho justo sobre la plataforma
       (values x plataforma-mas-alta)]
      
      [(> y NIVEL-ALTO)
       ;; Si se cae del mundo, poner en una posición segura
       (values x 700)]
      
      [else
       ;; Seguir cayendo
       (loop)])))


;; Verificar si está en el suelo
(define (en-suelo? x y)
  ;; Verifica si colisiona con alguna plataforma justo debajo de los pies
  (for/or ([p plataformas])
    (and (colision-bicho-plataforma? x (+ y 1) p)  ; +1 píxel de tolerancia
         (> (plataforma-y p) (- y 10))             ; La plataforma está cerca de los pies
         (< (plataforma-y p) (+ y 5)))))



;; ==========================
;; LOOP PRINCIPAL CORREGIDO
;; ==========================

(define (loop x y vy)
  (dibujar-escena x y)
  (sleep TIEMPO)
  
  ;; --- ANIMACIÓN ---
  (when (eq? current-h-key 'none)
    (set! anim-step 0))
  
  (when (not (eq? current-h-key 'none))
    (set! anim-step (modulo (+ anim-step 1) 3)))
  
  ;; --- MOVIMIENTO HORIZONTAL CON COLISIONES ---
  (define dx
    (cond [(eq? current-h-key 'left)  (- PASO)]
          [(eq? current-h-key 'right) PASO]
          [else 0]))
  
  ;; Intentar mover
  (define nueva-x (+ x dx))

  ;; Colisión con plataformas
  (define colision-h-plataformas?
    (for/or ([p plataformas])
      (colision-bicho-plataforma? nueva-x y p)))

  ;; Colisión con puertas que BLOQUEAN (no abiertas y sin globo del color)
  (define colision-h-puertas?
    (for/or ([pu puertas])
      (and (not (puerta-abierta? pu))
           (not (tiene-globo? (puerta-color pu))) ; si NO tengo el globo de ese color, bloquea
           (colision-bicho-con-rect?
            nueva-x y
            (puerta-x pu) (puerta-y pu)
            (puerta-ancho pu) (puerta-alto pu)))))

  (define colision-h? (or colision-h-plataformas? colision-h-puertas?))

  (define x-final
    (if colision-h?
        x
        (max (/ BICHO-ANCHO 2)
             (min nueva-x (- NIVEL-ANCHO (/ BICHO-ANCHO 2))))))

  ;; ========================
  ;; RECOGER GLOBOS
  ;; ========================
  (for-each
   (λ (g)
     (when (and (globo-activo? g)
                (colision-bicho-con-rect?
                 x-final y
                 (globo-x g) (globo-y g)
                 (globo-ancho g) (globo-alto g)))
       (agregar-globo! (globo-color g))
       (set-globo-activo?! g #f)))
   globos)

  ;; ========================
  ;; ABRIR PUERTAS (si tengo globo del color)
  ;; ========================
  (for-each
   (λ (pu)
     (when (and (not (puerta-abierta? pu))
                (tiene-globo? (puerta-color pu)) ; tengo globo del MISMO color
                (colision-bicho-con-rect?
                 x-final y
                 (puerta-x pu) (puerta-y pu)
                 (puerta-ancho pu) (puerta-alto pu)))
       ;; Abrir la puerta
       (set-puerta-abierta?! pu #t)
       ;; Si quieres que el globo se gaste, descomenta:
       ;; (usar-globo! (puerta-color pu))
       ))
   puertas)

  ;; --- SALTO ---
  (define vy1
    (cond
      [(and jump-request? (en-suelo? x-final y))
       (set! jump-request? #f)
       IMPULSO-SALTO]
      [else
       (set! jump-request? #f)  ; Esto evita saltos múltiples
       vy]))
  
  ;; --- GRAVEDAD Y MOVIMIENTO VERTICAL ---
  (define vy2 (+ vy1 GRAVEDAD))
  (define nueva-y (+ y vy2))
  
  ;; Colisión vertical con plataformas
  (define colision-v-plataformas?
    (for/or ([p plataformas])
      (colision-bicho-plataforma? x-final nueva-y p)))

  ;; Colisión vertical con puertas que BLOQUEAN
  (define colision-v-puertas?
    (for/or ([pu puertas])
      (and (not (puerta-abierta? pu))
           (not (tiene-globo? (puerta-color pu)))
           (colision-bicho-con-rect?
            x-final nueva-y
            (puerta-x pu) (puerta-y pu)
            (puerta-ancho pu) (puerta-alto pu)))))

  (define colision-v? (or colision-v-plataformas? colision-v-puertas?))

  (define-values (y-final vy-final)
    (cond
      [colision-v?
       (if (> vy2 0)  ; Estaba cayendo → piso
           (let ([plataforma-suelo
                  (for/fold ([suelo-y NIVEL-ALTO]) ([p plataformas])
                    (if (and (colision-bicho-plataforma? x-final nueva-y p)
                             (< (plataforma-y p) suelo-y))
                        (plataforma-y p)
                        suelo-y))])
             ;; Coloca los pies JUSTO sobre la plataforma
             (values (+ plataforma-suelo MARGEN-Y) 0))
           
           ;; Estaba subiendo → techo
           (let ([plataforma-techo
                  (for/fold ([techo-y 0]) ([p plataformas])
                    (define plat-bottom (+ (plataforma-y p) (plataforma-alto p)))
                    (if (and (colision-bicho-plataforma? x-final nueva-y p)
                             (> plat-bottom techo-y))
                        plat-bottom
                        techo-y))])
             ;; Poner la cabeza justo debajo del techo
             (values (+ plataforma-techo (- BICHO-ALTO MARGEN-Y) 1) 0)))]
      [else
       (values nueva-y vy2)]))
  
  ;; Limitar la posición Y
  (define y-limitada (max BICHO-ALTO (min y-final (- NIVEL-ALTO 10))))
  
  (loop x-final y-limitada vy-final))




;; ==========================
;; PANTALLA DE JUEGO
;; ==========================

(define (pantalla-juego)
  (let-values ([(x0 y0) (caida-inicial)])
    (loop x0 y0 0)))  ; Velocidad vertical inicial = 0





;; ===================================================
;; ==============  MENÚS Y CINEMÁTICAS  ==============
;; ===================================================

;; ==========================
;; PANTALLA PORTADA 1 (solo play)
;; ==========================

(define BTN-X 500)
(define BTN-Y 600)
(define BTN-W 280)
(define BTN-H 112)

(define (dibujar-fondo!)
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.71 1.0)))

(define (dibujar-boton-play!)
  ((draw-pixmap vent)
   "imagenes proyecto/Boton play y loading/play boton.bmp"
   (make-posn BTN-X BTN-Y)
   "black"))

(define (esperar-click-valido)
  (get-mouse-click vent))



;; ==========================
;; PANTALLA INICIO
;; ==========================

(define (pantalla-inicio)
  (clear-viewport vent)
  (dibujar-fondo!)
  (dibujar-boton-play!)
  (esperar-click-valido)
  (pantalla-portada))



;; ==========================
;; PANTALLA PORTADA 2
;; ==========================

(define (pantalla-portada)
  (clear-viewport vent)

  ;; Fondo azul
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.7 1.0))

  ;; Corazón con buhitos
  ((draw-pixmap vent)
   "imagenes proyecto/Buhos juntos/buhitos inicio.bmp"
   (make-posn 40 40)
   "black")

  ;; Logo FARAFALLA
  ((draw-pixmap vent)
   "imagenes proyecto/Logo/farafalla logo.bmp"
   (make-posn 418 370)
   "black")

  ;; Botón play
  ((draw-pixmap vent)
   "imagenes proyecto/Boton play solo/boton pantalla 2.bmp"
   (make-posn 520 600)
   "black")

  ;; Tu nombre
  ((draw-pixmap vent)
   "imagenes proyecto/Mi nombre/empleos.bmp"
   (make-posn 470 470)
   "black")

  (esperar-click-valido)
  (pantalla-cinematica-1)
  'ok)



;; ==========================
;; CINEMÁTICA 1
;; ==========================

(define (pantalla-cinematica-1)
  (clear-viewport vent)

  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ((draw-pixmap vent)
   "imagenes proyecto/Cinematica 1/texto 1.bmp"
   (make-posn 240 350)
   "black")

  (sonido-cinematica)
  (sleep 2.5)
  (pantalla-cinematica-2))



;; ==========================
;; CINEMÁTICA 2
;; ==========================

(define (pantalla-cinematica-2)
  (clear-viewport vent)

  (define x 400)
  (define y -150)

  ((draw-pixmap vent)
   "imagenes proyecto/Buhito/bicho de frente.bmp"
   (make-posn 400 -100)
   "black")

  (let loop ()
    (when (< y (+ HEIGHT 100))
      (clear-viewport vent)

      ((draw-solid-rectangle vent)
       (make-posn 0 0)
       WIDTH HEIGHT
       (make-rgb 0.25 0.7 1.0))

      ((draw-pixmap vent)
       "imagenes proyecto/Buhito/bicho de frente.bmp"
       (make-posn x y)
       "black")

      (set! y (+ y 10))
      (sleep 0.01)
      (loop)))

  (sleep 1)
  (pantalla-cinematica-3))



;; ==========================
;; CINEMÁTICA 3
;; ==========================

(define (pantalla-cinematica-3)
  (clear-viewport vent)

  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ((draw-pixmap vent)
   "imagenes proyecto/Cinematica 2/texto 2.bmp"
   (make-posn 240 370)
   "black")

  (sleep 2.5)
  (pantalla-cinematica-4))



;; ==========================
;; CINEMÁTICA 4
;; ==========================

(define (pantalla-cinematica-4)
  (clear-viewport vent)

  (define x 400)
  (define y -150)

  ((draw-pixmap vent)
   "imagenes proyecto/Buhito/bicho de cabeza.bmp"
   (make-posn 400 -100)
   "black")

  (let loop ()
    (when (< y (+ HEIGHT 100))
      (clear-viewport vent)

      ((draw-solid-rectangle vent)
       (make-posn 0 0)
       WIDTH HEIGHT
       (make-rgb 0.25 0.7 1.0))

      ((draw-pixmap vent)
       "imagenes proyecto/Buhito/bicho de cabeza.bmp"
       (make-posn x y)
       "black")

      (set! y (+ y 10))
      (sleep 0.01)
      (loop)))

  (sleep 1)
  (pantalla-cinematica-5))



;; ==========================
;; CINEMÁTICA 5 (FINAL)
;; ==========================

(define (pantalla-cinematica-5)
  (clear-viewport vent)

  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0 0 0))

  ((draw-pixmap vent)
   "imagenes proyecto/Logo/farafalla logo negro.bmp"
   (make-posn 200 250)
   "black")

  ((draw-pixmap vent)
   "imagenes proyecto/Mi nombre/nombre cinematica final.bmp"
   (make-posn 240 440)
   "black")

  (sleep 3.5)
  (pantalla-juego))



;; ==========================
;; INICIO DEL PROGRAMA
;; ==========================
 
(pantalla-inicio)
