;;;; cli.scm - Turing machine REPL at the command line

(include "engine.scm")
(include "global.scm")
(include "head.scm")
(include "parser.scm")
(include "rule.scm")
(include "state.scm")
(include "tape.scm")

(import (chicken format)
        (chicken io)
        (chicken port)
        (chicken process-context))

;; Convert TAPE into its string representation, omitting any leading and
;; trailing blank cells.
;; (tape->string tape) => string
(define (tape->string tape)
  (with-output-to-string
    (lambda ()
      (let ((first-char (tape-first-char tape))
            (last-char (tape-last-char tape)))
        (unless (null? first-char)
          (let loop ((head (make-head first-char)))
            (when (<= head last-char)
              (display (tape-read tape head))
              (loop (move-head head 'right)))))))))

;; Display TAPE, omitting any leading and trailing blank cells.
;; (display-tape tape) => unspecified
(define (display-tape tape)
  (display (tape->string tape)))

(define (main path)
  (define program-string (with-input-from-file path
                           (lambda ()
                             (apply string-append
                                    (map (lambda (e)
                                           (string-append e (string #\newline)))
                                         (read-lines))))))
  (define transition-table (parse-program program-string))
  (define current-state)
  (define head)
  (define (reset!)
    (set! current-state (make-state "0"))
    (set! head (make-head 0)))
  (define (evaluate! tape)
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
                   (set! current-state error-state))
            (begin (set! current-state (rule-next-state rule))
                   (set! tape (tape-write tape head (rule-write-symbol rule)))
                   (set! head (let ((dir (rule-move-direction rule)))
                                (cond ((eqv? dir left-character)
                                       (move-head head 'left))
                                      ((eqv? dir right-character)
                                       (move-head head 'right))
                                      (else head)))))))
      (unless (halt-state? current-state)
        (eval-loop)))
    tape)
  (display program-string)
  (let repl-loop ((repl-i 0))
    (reset!)
    (format #t "~A> " repl-i)
    (let ((output-tape (evaluate! (make-tape (read-line)))))
      (format #t "=> ~A, ~A~%" current-state (tape->string output-tape))
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
