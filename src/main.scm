;;;; main.scm - Turing machine simulator at the command line.

(declare (uses display)
         (uses engine)
         (uses tape))

(import (chicken format)
        (chicken io)
        (chicken process-context))

;; Evaluate a single input.
;; (tm-eval!) -> void
(define (tm-eval! #!optional (line (read-line)))
  (engine-reset! line)
  (let loop ()
    (display-configs (engine-configs))
    (engine-step!)
    (unless (engine-done?)
      (loop)))
  (display-configs (engine-configs)))

;; Evaluate inputs in a REPL.
;; (tm-repl!) -> void
(define (tm-repl! #!optional (i 1))
  (format #t "~A> " i)
  (let ((line (read-line)))
    (when (eof-object? line)
      (newline)
      (exit))
    (tm-eval! line))
  (tm-repl! (+ i 1)))

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
      (tm-eval!)
      (tm-repl!)))

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
