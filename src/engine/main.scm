(declare (uses tape)
         (uses transition))

(import (chicken io)
        (chicken process-context))

(define table)                          ; Transition table
(define state)                          ; Current state
(define head)                           ; Read/write head

;; Reset state and head to their initial values.
(define (reset)
  (set! state #\0)
  (set! head (make-head 0)))

(define (main path)
  (display-program path)
  (set! table (parse-transition-table path))
  (let repl-loop ((repl-i 0))
    (reset)
    (display repl-i) (display "> ")
    (let ((tape (make-tape (read-line))))
      (let eval-loop ()
        (let ((rule (evaluate-transition table state (tape-read tape head))))
          (set! state (next-state rule))
          (set! tape (tape-write tape head (write-symbol rule)))
          (set! head (case (move-direction rule)
                       ((#\L) (move-head head 'left))
                       ((#\R) (move-head head 'right))
                       (else head))))
        (unless (or (char=? state #\A)
                    (char=? state #\R))
          (eval-loop)))
      (display "=> ")
      (display state)
      (display ", ")
      (display-tape tape)
      (newline)
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
