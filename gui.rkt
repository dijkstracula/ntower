#lang racket

(require racket/gui
         racket/draw
         "racket-tower.rkt")

;;; default scale factor.  Probably a constant.
(define pixels-per-gameplay-unit 50)



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

(define rock-stipple (new brush%
                       [stipple (read-bitmap "rock.png")]))

(define (between val x y)
  (and (>= val x) (< val y)))

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
  

;;;;;;;;;;;;;;;;;; canvas classes ;;;;;;;;;;;;;;;;;;;

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
      (let ([vp (get-viewport)])
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
          ; Draw the sky
          (send dc set-brush (new brush% [color sky-colour]))
          (send dc draw-rectangle (viewport-x vp) (viewport-y vp) (viewport-width vp) (viewport-height vp))
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
    
    ; get-viewport: -> viewport
    ; produces the active viewport.
    (define/private (get-viewport)
      (define-values (x y) (get-view-start))
      (define-values (w h) (get-client-size))
      (viewport x y w h))))



(define tower-gameplay-window%
  (class object%
    (init-field state)
    
    ;gameplay window
    (define frame
      (new frame%
           [label "Nathan Towers"]
           [width 800]
           [height 600]))
    
    ;viewport
    (define canvas
      (new render-canvas%
           [parent frame]
           [paint-callback
            (λ (canvas dc)
              (send canvas render dc state))]))
    
    ; Centre the viewport in the centre of the ground floor
    (send canvas init-auto-scrollbars
          (- (* pixels-per-gameplay-unit GAMEPLAY-WIDTH) 400)
          (- (* pixels-per-gameplay-unit GAMEPLAY-LEVELS) 500)
          0.5 (/ AGROUND-LEVELS GAMEPLAY-LEVELS))
    
    ; show everything
    (send frame show #t)
    
    (super-new)))

(define game-state (new game-state%))

(new tower-gameplay-window% [state game-state])