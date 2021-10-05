;;;; cli.scm - Turing machine REPL at the command line

(include "engine.scm")
(include "global.scm")
(include "parser.scm")
(include "rule.scm")

(import (chicken format)
        (chicken io)
        (chicken process-context))

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
  (display program-string)
  (setup-engine blank-character)
  (let repl-loop ((repl-i 0))
    (reset!)
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
        (unless (or (state=? current-state accept-state)
                    (state=? current-state reject-state)
                    (state=? current-state error-state))
          (eval-loop)))
      (display "=> ")
      (display current-state)
      (display ", ")
      (display-tape tape)
      (newline)
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
