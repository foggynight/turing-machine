;;;; display.scm - Turing machine display procedures.

(declare (unit display)
         (uses config)
         (uses state)
         (uses utils))

(import (chicken format))

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
  (format #t ";;; ~A~%" (make-string 76 #\-)))

;; Display the STEP number within a separator.
;; (display-step integer) -> void
(define (display-step step)
  (define len (string-length (number->string step)))
  (format #t ";;; ~A ~A~%"
          step
          (make-string (- 75 len) #\-)))

;; Display the configuration tree given by CONFIGS.
;; (display-configs tree) -> void
(define (display-configs configs)
  (define corner     "+-- ")
  (define empty      "    ")
  (define horizontal "|-- ")
  (define vertical   "|   ")
  (define (walk children #!optional (prefix ""))
    (unless (null? children)
      (let ((child (car children))
            (last (null? (cdr children))))
        (format #t "~A~A~%"
                (string-append prefix (if last corner horizontal))
                (config->string (tree-root child)))
        (walk (tree-children child)
              (string-append prefix (if last empty vertical)))
        (walk (cdr children) prefix))))
  (format #t "~A~%" (config->string (tree-root configs)))
  (walk (tree-children configs)))

;; Display the results of an evaluation.
;; (display-results list integer) -> void
(define (display-results configs steps)
  (define (get-halts lst)
    (if (null? lst)
        '()
        (let ((state (config-state (car lst))))
          (if (halt-state? state)
              (cons state (get-halts (cdr lst)))
              (get-halts (cdr lst))))))
  (define (display-final-configs configs)
    (define final-configs (map tree-root (tree-leaves configs)))
    (format #t "~A~%" (config->string (car final-configs)))
    (for-each (lambda (c) (format #t "       ~A~%" (config->string c)))
              (cdr final-configs)))
  (format #t ";;; RESULTS ~A~%~
              Halts: ~A~%~
              Steps: ~A~%~
              Final: "
          (make-string 68 #\-)
          (list-unique (get-halts (tree-preorder configs)))
          steps)
  (display-final-configs configs)
  (format #t ";;; ~A~%" (make-string 76 #\-)))
