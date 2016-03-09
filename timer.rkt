#lang racket/gui

(provide
 (all-defined-out))

;;; create-looper: int (void -> void) -> (int -> void)
; Produces a function that, every <fps>, invokes a function
; that consumes the number of millis since the last invocation
; and calls a callback.
(define (create-looper fps fn)
  (letrec ([looper-fn
            (λ (old-millis)
              (let ([new-millis (current-milliseconds)])
                (yield)
                (if (> (- new-millis old-millis) (/ 1000 fps))
                    (begin (fn (- new-millis old-millis))
                           (queue-callback (thunk (looper-fn new-millis)) #t))
                    (queue-callback (thunk (looper-fn old-millis)) #t))))])
    looper-fn))