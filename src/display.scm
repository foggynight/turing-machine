;;;; display.scm - Turing machine display procedures.

(declare (unit display)
         (uses config)
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
  (format #t ";; ~A~%" (make-string 77 #\-)))

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
