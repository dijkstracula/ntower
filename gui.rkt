#lang racket

(require racket/gui
         racket/draw
         "ntower.rkt"
         "timer.rkt")

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
(define evening-colour (make-object color% 80 59 8))
(define night-colour (make-object color% 9 16 196))

(define space-colour (make-object color% 5 5 20))

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
  (and (>= val x) (<= val y)))

;;; draw-skyline: dc dx bitmap
; draw the bitmap at the ground level.
(define (draw-skyline dc dx bitmap)
  (let ([brush (new brush% [stipple bitmap])])
    (send dc set-brush brush)
    (send dc draw-rectangle
          0 (- (* pixels-per-gameplay-unit AGROUND-LEVELS) (send bitmap get-height))
          dx (send bitmap get-height))))

;;; lerp
; produce a mix of the two values
(define (lerp x y n)
  (+ (* x (- 1 n))
     (* y n)))

;;; colour-lerp
; produce a mix of the two supplied colours.
(define (colour-lerp c1 c2 n)
  (let ([r (lerp (send c1 red) (send c2 red) n)]
        [g (lerp (send c1 green) (send c2 green) n)]
        [b (lerp (send c1 blue) (send c2 blue) n)])
    (make-object color% (exact-truncate r) (exact-truncate g) (exact-truncate b))))

;;; colour-3lerp
; produce a mix of the three supplied colours, where [0,0.5] mixes
; between c1 and c2 and [0.5, 1] m(ixes between c2 and c3
(define (colour-3lerp c1 c2 c3 n)
  (cond
    [(between n 0.0 0.5) (colour-lerp c1 c2 (* n 2))]
    [(between n 0.5 1.0) (colour-lerp c2 c3 (* (- n 0.5) 2))]))


;;; sky-colour time -> color%
; Produce the colour of the sky given a time of day.
; TODO: night/day cycles
(define (sky-colour time)
  (cond
    [(between time 0 MORNING_BEGIN) night-colour]
    [(between time MORNING_BEGIN MORNING_END) (colour-3lerp night-colour morning-colour day-colour
                                                            (/ (- time MORNING_BEGIN)
                                                               (- MORNING_END MORNING_BEGIN)))]
    [(between time DAY_BEGIN DAY_END) day-colour]
    [(between time EVENING_BEGIN EVENING_END) (colour-3lerp day-colour evening-colour night-colour
                                                            (/ (- time EVENING_BEGIN)
                                                               (- EVENING_END EVENING_BEGIN)))]
    [(between time NIGHT_BEGIN (* 24 60)) night-colour]))

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
               [sky-colour (colour-lerp space-colour
                                        (sky-colour (send state get-time))               
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
              (+ x-radius (* x-radius hand-scale (cos ang)))
              (+ y-radius (* y-radius hand-scale (- 0 (sin ang)))))
        p))
    
    ; draw-clock dc time time -> void
    (define/public (draw-clock dc hour min)
      (send dc draw-ellipse 0 0 (get-width) (get-height))
      (send dc draw-path (clock-hand-path (time-to-arc (/ hour 12)) 0.5))
      (send dc draw-path (clock-hand-path (time-to-arc (/ min 60)) 0.8)))))

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
    
    (define map-frame
      (new frame%
           [label "Map"]
           [width 300]
           [height (* 300 (/ GAMEPLAY-LEVELS GAMEPLAY-WIDTH))]))
    (define map-canvas
      (new map-canvas%
           [parent map-frame]
           [paint-callback
            (λ (canvas dc)
              (send canvas render dc state (send gameplay-canvas get-viewport/world)))]))
    
    
    (define info-frame
      (new frame%
           [label "Info"]))
    (define info-hpanel
      (new horizontal-panel%
           [parent info-frame]))
    (define clock-canvas
      (new clock-canvas%
           [parent info-hpanel]
           [min-width 100]
           [min-height 100]
           [paint-callback
            (λ (canvas dc)
              (let* ([min (modulo (send state get-time) 60)]
                     [hour (+ (/ min 60) (truncate (/ (send state get-time) 60)))])
                (send canvas draw-clock dc hour min)))]))
    (define money-canvas
      (new canvas%
           [parent info-hpanel]
           [min-width 400]
           [min-height 100]
           [paint-callback
            (λ (canvas dc)
              (send dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
              (send dc set-font (make-font #:size 20 #:family 'roman #:weight 'bold))
              (send dc draw-text
                    (format "Day ~a" (number->string (send state get-day))) 10 10)
              (send dc draw-text
                    (format "¶~a" (number->string (send state get-money))) 110 10)
              (send dc draw-text
                    (format "Status: ~a" (send state get-status-msg)) 10 60))]))
    
    
    
    ;Centre the viewport in the centre of the ground floor
    (send gameplay-canvas init-auto-scrollbars
          (* pixels-per-gameplay-unit GAMEPLAY-WIDTH)
          (* pixels-per-gameplay-unit GAMEPLAY-LEVELS)
          0.5
          (- (/ AGROUND-LEVELS GAMEPLAY-LEVELS) .06))
    
    ; show everything
    (send gameplay-frame show #t)
    (send map-frame show #t)
    (send info-frame show #t)
    
    (define looper (create-looper 30
                                  (λ (millis)
                                    (begin
                                      (send state increment-time (exact-truncate (/ millis 30)))
                                      (send gameplay-canvas refresh)
                                      (send map-canvas refresh)
                                      (send info-frame refresh)))))
    (looper (current-milliseconds))
    
    (super-new)))


(define game-state (new game-state%))
(new tower-gui [state game-state])