;;;; rule.scm - Turing machine transition rule functions.

;; TODO Describe rule layout.

(include "utils/list.scm")

(define (rule-ref rule index)
  (list-ref rule index))

(define (rule-set! rule index value)
  (set! (list-ref rule index) value))

;; Copy a rule.
;; (rule-copy rule) -> rule
(define (rule-copy rule)
  (list-copy rule))

;; Transition rule accessor functions.
(define (rule-current-state rule) (list-ref rule 0))
(define (rule-read-symbol rule) (list-ref rule 1))
(define (rule-next-state rule) (list-ref rule 2))
(define (rule-write-symbol rule) (list-ref rule 3))
(define (rule-move-direction rule) (list-ref rule 4))
