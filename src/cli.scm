;;;; cli.scm - Turing machine REPL at the command line

(include "utils/string.scm")
(include "utils/vector.scm")
(include "engine.scm")

(import (chicken format)
        (chicken io)
        (chicken process-context))

(define comment-character #\;)
(define blank-character #\_)

(define accept-character #\A)
(define reject-character #\R)
(define error-character #\E)

(define left-character #\L)
(define right-character #\R)
(define stay-character #\S)

;; Parse the program contained within the file at PATH.
;; (parse-program string) => list
(define (parse-program path)
  (define (aux path table)
    (with-input-from-file path
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            (unless (char=? (string-ref line 0) comment-character)
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

;; Display TAPE, omitting any leading and trailing blank cells.
;; (display-tape tape) => unspecified
(define (display-tape tape)
  (let ((first-char (tape-first-char tape))
        (last-char (tape-last-char tape)))
    (unless (null? first-char)
      (let loop ((head (make-head first-char)))
        (when (<= head last-char)
          (display (tape-read tape head))
          (loop (move-head head 'right)))))))

(define (main path)
  (define transition-table)
  (define current-state)
  (define head)
  (define (reset)
    (set! current-state (make-state #\0))
    (set! head (make-head 0)))
  (display-program path)
  (set! transition-table (parse-program path))
  (setup-engine blank-character)
  (let repl-loop ((repl-i 0))
    (reset)
    (display repl-i) (display "> ")
    (let ((tape (make-tape (read-line))))
      (let eval-loop ()
        (let* ((read-symbol (tape-read tape head))
               (rule (evaluate-transition transition-table
                                          current-state
                                          read-symbol)))
          (if (null? rule)
              (begin (format #t "Error: No rule found for:~%~
                                 - Current state = ~A~%~
                                 - Read symbol = ~A~%"
                             current-state read-symbol)
                     (set! current-state error-character))
              (begin (set! current-state (rule-next-state rule))
                     (set! tape (tape-write tape head (rule-write-symbol rule)))
                     (set! head (let ((dir (rule-move-direction rule)))
                                  (cond ((eqv? dir left-character)
                                         (move-head head 'left))
                                        ((eqv? dir right-character)
                                         (move-head head 'right))
                                        (else head)))))))
        (unless (or (char=? current-state accept-character)
                    (char=? current-state reject-character)
                    (char=? current-state error-character))
          (eval-loop)))
      (display "=> ")
      (display current-state)
      (display ", ")
      (display-tape tape)
      (newline)
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
