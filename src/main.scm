;;;; main.scm - Turing machine simulator at the command line.

(declare (uses engine)
         (uses tape))

(import (chicken format)
        (chicken io)
        (chicken process-context))

;; Display the given program, with separators above and below, and the separator
;; above containing PATH.
;; (display-program string string) -> void
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

;; TODO Use TM-EVAL in TM-REPL.

;; Evaluate a single input.
;; (tm-eval) -> void
(define (tm-eval)
  (define line (read-line))
  (engine-reset! line)
  (engine-skip!)
  (format #t "-> ~A, ~A~%"
          (config-state (car (engine-configs)))
          (tape->string (car (config-tapes (car (engine-configs)))))))

;; Evaluate inputs in a REPL.
;; (tm-repl) -> void
(define (tm-repl #!optional (i 0))
  (format #t "~A> " i)
  (let ((line (read-line)))
    (when (eof-object? line)
      (newline)
      (exit))
    (engine-reset! line))
  (engine-skip!)
  (format #t "-> ~A, ~A~%"
          (config-state (car (engine-configs)))
          (tape->string (car (config-tapes (car (engine-configs))))))
  (tm-repl (+ i 1)))

(define (main mode path)
  (define program-string
    (with-input-from-file path
      (lambda ()
        (apply string-append
               (map (lambda (e)
                      (string-append e (string #\newline)))
                    (read-lines))))))
  (when (eq? mode 'repl)
    (display-program path program-string))
  (engine-init! program-string)
  (if (eq? mode 'eval)
      (tm-eval)
      (tm-repl)))

(let* ((args (command-line-arguments))
       (len (length args)))
  (cond ((zero? len) (format #t "Error: No arguments~%") (exit 1))
        ((= len 1) (main 'eval (car args)))
        ((= len 2) (let ((mode (string->symbol (car args))))
                     (if (or (eq? mode 'eval)
                             (eq? mode 'repl))
                         (main mode (cadr args))
                         (begin (format #t "Error: Invalid MODE: ~A~%" mode)
                                (exit 1)))))
        (else (format #t "Error: Too many arguments~%") (exit 1))))
