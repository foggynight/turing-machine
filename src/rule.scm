;;;; rule.scm - Turing machine transition rule functions

;; TODO Add comment to describe rule layout.

;; Transition rule accessor functions.
(define (rule-current-state rule) (list-ref rule 0))
(define (rule-read-symbol rule) (list-ref rule 1))
(define (rule-next-state rule) (list-ref rule 2))
(define (rule-write-symbol rule) (list-ref rule 3))
(define (rule-move-direction rule) (list-ref rule 4))
