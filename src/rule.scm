;;;; rule.scm - Turing machine transition rule functions.

(include "utils/list.scm")

(define (rule-ref rule index)
  (list-ref rule index))

(define (rule-set! rule index value)
  (set-car! (list-tail rule index) value))

;; Get a copy of RULE.
;; (rule-copy rule) -> rule
(define (rule-copy rule)
  (list-copy rule))

;; Transition rule accessor functions.
(define (rule-current-state rule) (rule-ref rule 0))
(define (rule-read-symbol rule) (rule-ref rule 1))
(define (rule-next-state rule) (rule-ref rule 2))
(define (rule-write-symbol rule) (rule-ref rule 3))
(define (rule-move-direction rule) (rule-ref rule 4))
