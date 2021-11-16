;;;; types.scm - Turing machine type definitions.

(define-type head number)
(define-type rule (struct rule))
(define-type state string)
(define-type tape vector)
