;;;; main.scm - Turing machine simulator at the command line.

(declare (uses engine)
         (uses tape))

(import (chicken format)
        (chicken io)
        (chicken process-context))

;; Display the given program, with separators above and below, and the separator
;; above containing PATH.
(: display-program (string string -> void))
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

(: main (string -> void))
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
    (format #t "~A> " repl-i)
    (let ((line (read-line)))
      (when (eof-object? line)
        (newline)
        (exit))
      (engine-reset! line)
      (engine-skip!)
      (format #t "-> ~A, ~A~%"
              (engine-state)
              (tape->string (car (engine-tapes))))
      (repl-loop (+ repl-i 1)))))

(main (car (command-line-arguments)))
