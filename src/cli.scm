;;;; cli.scm - Turing machine REPL at the command line

(include "engine.scm")

(import (chicken io)
        (chicken process-context))

(define table)                          ; Transition table
(define state)                          ; Current state
(define head)                           ; Read/write head

;; Parse the program contained within the file at PATH.
;; (parse-program string) => list
(define (parse-program path)
  (define (aux path table)
    (with-input-from-file path
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            (unless (char=? (string-ref line 0) #\;)
              (set! table (cons (parse-rule line) table)))
            (loop (read-line))))))
    table)
  (reverse (aux path '())))

;; Display the program contained within the file at PATH.
;; (display-program string) => unspecified
(define (display-program path)
  (with-input-from-file path
    (lambda ()
      (let loop ((line (read-line)))
        (unless (eof-object? line)
          (display line)
          (newline)
          (loop (read-line)))))))

;; Reset state and head to their initial values.
(define (reset)
  (set! state (make-state #\0))
  (set! head (make-head 0)))

(define (main path)
  (display-program path)
  (set! table (parse-program path))
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
