#lang racket

(require racket/gui
         racket/draw
         "ntower.rkt")

;;; default scale factor.  Probably a constant.
(define pixels-per-gameplay-unit 64)



;;;;;;;;;;;;;;; viewport and clipping ;;;;;;;;;;;;;;;

;;; viewport
; The region we will render.
(define-struct viewport (x y width height) #:transparent)

;;; viewport-center: viewport -> (x,y)
; Produces the point in the middle of a viewport
(define (viewport-center vp)
  (values (+ (viewport-x vp) (/ (viewport-width vp) 2))
          (+ (viewport-y vp) (/ (viewport-height vp) 2))))


;;;;;;;;;;;;;;;;;; sky rendering ;;;;;;;;;;;;;;;;;;;

(define morning-colour (make-object color% 255 252 163))
(define day-colour (make-object color% 163 232 255))
(define evening-colour (make-object color% 212 59 8))
(define night-colour (make-object color% 9 16 196))

(define space-colour (make-object color% 5 5 50))

(define skyline0-bitmap (read-bitmap "textures/skyline0.png"))
(define skyline1-bitmap (read-bitmap "textures/skyline1.png"))
(define skyline2-bitmap (read-bitmap "textures/skyline2.png"))

; texture for gameplay window
(define rock-stipple (new brush%
                          [stipple (read-bitmap "textures/rock.png")]))

; rock colour for map window
(define rock-brush (new brush%
                        [color (make-object color% 145 124 6)]))

(define (between val x y)
  (and (>= val x) (< val y)))

;;; draw-skyline: dc dx bitmap
; draw the bitmap at the ground level.
(define (draw-skyline dc dx bitmap)
  (let ([brush (new brush% [stipple bitmap])])
    (send dc set-brush brush)
    (send dc draw-rectangle
          0 (- (* pixels-per-gameplay-unit AGROUND-LEVELS) (send bitmap get-height))
          dx (send bitmap get-height))))

;;; colour-lerp
; produce a mix of the two supplied colours.
(define (colour-lerp c1 c2 n)
  (let ([r (+ (* (send c1 red) n)
              (* (send c2 red) (- 1 n)))]
        [g (+ (* (send c1 green) n)
              (* (send c2 green) (- 1 n)))]
        [b (+ (* (send c1 blue) n)
              (* (send c2 blue) (- 1 n)))])
    (make-object color% (truncate r) (truncate g) (truncate b))))


;;; sky-colour time -> color%
; Produce the colour of the sky given a time of day.
; TODO: night/day cycles
(define (sky-colour time)
  day-colour)

;;;;;;;;;;;;;;;;;; clock rendering ;;;;;;;;;;;;;;;;;;;


; time-to-arc time -> radian
(define (time-to-arc t)
  (- (/ pi 2)
     (* t 2 pi)))

;;;;;;;;;;;;;;;;;; canvas classes ;;;;;;;;;;;;;;;;;;;

;;; map-canvas
(define map-canvas%
  (class canvas%
    (inherit get-width
             get-height
             get-view-start
             get-client-size
             refresh)
    (super-new)
    
    (define/public (render dc state vp)
      (render-background dc state)
      (render-viewport dc vp))
    
    (define/private (render-background dc state)
      (let ([ground (* (get-height) (/ AGROUND-LEVELS GAMEPLAY-LEVELS))])
        (send dc set-pen "black" 0 'transparent)
        ; Draw the sky
        (send dc set-brush (new brush% [color (sky-colour (send state get-time))]))
        (send dc draw-rectangle 0 0 (get-width) ground)
        ; draw the ground
        (send dc set-brush rock-brush)
        (send dc draw-rectangle 0 ground (get-width) (- (get-height) ground))))
    
    (define/private (render-viewport dc vp)
      (let ([x (* (get-width) (/ (viewport-x vp) GAMEPLAY-WIDTH))]
            [y (* (get-height) (/ (viewport-y vp) GAMEPLAY-LEVELS))]
            [w (* (get-width) (/ (viewport-width vp) GAMEPLAY-WIDTH))]
            [h (* (get-height) (/ (viewport-height vp) GAMEPLAY-LEVELS))])
        (send dc set-pen "black" 1 'solid)
        (send dc set-brush "black" 'transparent)
        (send dc draw-rectangle x y w h)))))

;;; render-canvas
; The region we will render the game into.
(define render-canvas%
  (class canvas%
    (inherit get-width
             get-height
             get-view-start
             get-client-size
             refresh)
    (super-new [style '(vscroll hscroll)])
    
    (field
     [needs-redraw #f])
    
    ; render: dc state -> void
    (define/public (render dc state)
      (let ([vp (get-viewport/screen)])
        (send dc set-clipping-rect
              (viewport-x vp) (viewport-y vp)
              (viewport-width vp) (viewport-height vp))
        (render-background vp dc state)
        (render-debug vp dc state)))
    
    ;;;;;;;; render routines
    
    ;;; render-background viewport dc state -> void
    (define/private (render-background vp dc state)
      (let-values ([(vx vy) (viewport-center vp)]
                   [(dx dy) (send dc get-size)])
        (let* ([ratio (/ vy dy)]
               [sky-colour (colour-lerp (sky-colour (send state get-time))
                                        space-colour
                                        ratio)])
          (send dc set-pen "black" 0 'transparent)
          ; Draw the sky
          (send dc set-brush (new brush% [color sky-colour]))
          (send dc draw-rectangle (viewport-x vp) (viewport-y vp) (viewport-width vp) (viewport-height vp))
          ; Draw background cityscape
          (draw-skyline dc dx skyline0-bitmap)
          (draw-skyline dc dx skyline1-bitmap)
          (draw-skyline dc dx skyline2-bitmap)
          ; Draw the ground
          (send dc set-brush rock-stipple)
          (send dc draw-rectangle
                0 (* pixels-per-gameplay-unit AGROUND-LEVELS)
                dx vy))))
    
    ; render-debug: viewport dc -> void
    ; Renders debug information for Nathan.
    (define/private (render-debug viewport dc state)
      (send dc set-text-background "blue")
      (define-values (w h) (get-view-start))
      (send dc draw-text (~v viewport) w h))
    
    ;;;;;;;;; helpers
    
    ; get-viewpor/screent: -> viewport
    ; produces the active viewport in screen coordinates (e.g. pixels)
    (define/private (get-viewport/screen)
      (define-values (x y) (get-view-start))
      (define-values (w h) (get-client-size))
      (viewport x y w h))
    
    ; get-viewport/world: -> viewport
    ; produces the active viewport in world coordinates
    (define/public (get-viewport/world)
      (define-values (x y) (get-view-start))
      (define-values (w h) (get-client-size))
      (viewport (/ x pixels-per-gameplay-unit)
                (/ y pixels-per-gameplay-unit)
                (/ w pixels-per-gameplay-unit)
                (/ h pixels-per-gameplay-unit)))))

(define clock-canvas%
  (class canvas%
    (inherit get-width
             get-height
             get-view-start
             get-client-size
             refresh)
    (super-new)
    
    ; clock-hand-path angle float -> path
    (define/private (clock-hand-path ang hand-scale)
      (let* ([p (new dc-path%)]
             [x-radius (/ (get-width) 2)]
             [y-radius (/ (get-height) 2)])
        (send p move-to x-radius y-radius)
        (send p line-to
              (+ x-radius (* hand-scale (cos ang)))
              (+ y-radius (* hand-scale y-radius (- (sin ang) 0))))
        p))
    
    ; draw-clock dc time time -> void
    (define/public (draw-clock dc hour min)
      (send dc draw-ellipse 0 0 (get-width) (get-height))
      (print hour)
      ; (send dc draw-path (clock-hand-path (time-to-arc (/ hour 12)) 0.8))
      (send dc draw-path (clock-hand-path (time-to-arc (/ min 60)) 0.5)))))

(define tower-gui
  (class object%
    (init-field state)
    
    ;gameplay window
    (define gameplay-frame
      (new frame%
           [label "Nathan Towers"]
           [width 800]
           [height 600]))
    (define gameplay-canvas
      (new render-canvas%
           [parent gameplay-frame]
           [paint-callback
            (λ (canvas dc)
              (send canvas render dc state)
              (send map-canvas on-paint))]))
    
    (define map
      (new frame%
           [label "Map"]
           [width 300]
           [height (* 300 (/ GAMEPLAY-LEVELS GAMEPLAY-WIDTH))]))
    (define map-canvas
      (new map-canvas%
           [parent map]
           [paint-callback
            (λ (canvas dc)
              (send canvas render dc state (send gameplay-canvas get-viewport/world)))]))
    
    
    (define info
      (new frame%
           [label "Info"]))
    (define clock-canvas
      (new clock-canvas%
           [parent info]
           [min-width 100]
           [min-height 100]
           [paint-callback
            (λ (canvas dc)
              (let* ([hour (truncate (/ (send state get-time) 60))]
                     [min (modulo (send state get-time) 60)])
                (send canvas draw-clock dc hour min)))]))
    
    
    
    ;Centre the viewport in the centre of the ground floor
    (send gameplay-canvas init-auto-scrollbars
          (* pixels-per-gameplay-unit GAMEPLAY-WIDTH)
          (* pixels-per-gameplay-unit GAMEPLAY-LEVELS)
          0.5
          (- (/ AGROUND-LEVELS GAMEPLAY-LEVELS) .06))
    
    ; show everything
    (send gameplay-frame show #t)
    (send map show #t)
    (send info show #t)
    
    (super-new)))


(define game-state (new game-state%))
(new tower-gui [state game-state])