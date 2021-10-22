;;;; cli.scm - Turing machine REPL at the command line.

(include "engine.scm")
(include "global.scm")
(include "head.scm")
(include "tape.scm")

(import (chicken format)
        (chicken io)
        (chicken port)
        (chicken process-context))

;; Display the given program, with separators above and below, and the separator
;; above containing PATH.
;; (display-program string string) -> unspecified
(define (display-program path program-string)
  (let* ((path-length (string-length path))
         (separator-length (- 74 path-length)))
    (format #t ";;;; ~A ~A~%"
            path (make-string (if (negative? separator-length)
                                  0
                                  separator-length)
                              #\-)))
  (display program-string)
  (format #t ";; ~A~%" (make-string 77 #\-)))

;; Convert TAPE into its string representation, omitting any leading and
;; trailing blank cells.
;; (tape->string tape) -> string
(define (tape->string tape)
  (with-output-to-string
    (lambda ()
      (let ((first-char (tape-first-char tape))
            (last-char (tape-last-char tape)))
        (unless (null? first-char)
          (let loop ((head first-char))
            (when (<= head last-char)
              (display (tape-read tape head))
              (loop (move-head head 'right)))))))))

(define (main path)
  (define program-string
    (with-input-from-file path
      (lambda ()
        (apply string-append
               (map (lambda (e)
                      (string-append e (string #\newline)))
                    (read-lines))))))
  (display-program path program-string)
  (engine-init! program-string)
  (let repl-loop ((repl-i 0))
    (engine-reset!)
    (format #t "~A> " repl-i)
    (let ((output-tape (engine-skip! (make-tape (read-line)))))
      (format #t "-> ~A, ~A~%" (engine-state) (tape->string output-tape))
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
