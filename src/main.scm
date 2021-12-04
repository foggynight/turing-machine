;;;; main.scm - Turing machine simulator at the command line.

(declare (uses display)
         (uses engine)
         (uses tape))

(import (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken time))

;; Evaluate a single input.
;; (tm-eval! string) -> void
(define (tm-eval! #!optional (line (read-line)))
  (engine-reset! line)
  (let ((start-time (current-process-milliseconds)))
    (let loop ((step 0)  ; Current evaluation step.
               (time 0)  ; Start time of current step.
               (last 0)) ; Start time of previous step.
      (if (or (engine-done?)
              (if (negative? (limit-step)) #f (> step (limit-step)))
              (if (negative? (limit-time)) #f (> time (limit-time))))
          (display-results (engine-configs) (- step 1) last)
          (begin (unless (zero? step)
                   (engine-step!))
                 (display-step+time step time)
                 (display-configs (engine-configs))
                 (loop (+ step 1)
                       (- (current-process-milliseconds)
                          start-time)
                       time))))))

;; Evaluate inputs in a REPL.
;; (tm-repl! integer) -> void
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
