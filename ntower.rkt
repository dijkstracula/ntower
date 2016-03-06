#lang racket

(provide
 (all-defined-out))

;;;;;;;;;;;;;;; core constants ;;;;;;;;;;;;;;;

; How many above/below ground levels we have
(define AGROUND-LEVELS 100)
(define UGROUND-LEVELS 20)

(define GAMEPLAY-LEVELS (+ AGROUND-LEVELS UGROUND-LEVELS))
(define GAMEPLAY-WIDTH 50)

(define WIDTH-UNITS 3000)




;;;;;;;;;;;;;;;;;;; time ;;;;;;;;;;;;;;;;;;;

(define MORNING_BEGIN (* 5 60))  ; 5 AM
(define DAY_BEGIN     (* 9 60))  ; 9 AM
(define EVENING_BEGIN (* 19 60)) ; 7 PM
(define NIGHT_BEGIN   (* 22 60)) ; 10 PM

(define NIGHT_END MORNING_BEGIN)
(define MORNING_END DAY_BEGIN)
(define DAY_END EVENING_BEGIN)
(define EVENING_END NIGHT_BEGIN)




;;;;;;;;;;;;;;; state objects ;;;;;;;;;;;;;;;

; TODO: contracts?
(define game-state%
  (class object%
  
  (field
   ; The current time of day, in seconds.  Game starts at 5 AM.
   [time (* 5 60)]

   ; The current day.
   [day 0])
  
  (define MAX-TIME 3)
  
  (define/public (tick steps)
    (let ([t (+ time steps)])
      (if (< t MAX-TIME)
          (set! time t)
          (let* ([new-time (modulo t MAX-TIME)]
                 [new-day  (quotient t MAX-TIME)])
            (begin
              (set! time new-time)
              (set! day new-day))))))

    ; accessors
    (define/public (get-time) time)
    (define/public (get-day) day)

  (super-new)))