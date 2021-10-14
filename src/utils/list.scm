;;;; list.scm - List utility functions

;; Copy a list.
;; (list-copy list) -> list
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (list-copy (cdr lst)))))
