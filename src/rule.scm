;;;; rule.scm - Turing machine transition rule functions.

(declare (unit rule))

(import (srfi 1))

;; Record type representing a transition rule.
(define-record rule
  current-state
  read-symbols
  next-state
  write-symbols
  move-directions)

;; Get a deep copy of a rule.
;; (rule-copy rule) -> rule
(define (rule-copy rule)
  (make-rule (rule-current-state rule)
             (list-copy (rule-read-symbols rule))
             (rule-next-state rule)
             (list-copy (rule-write-symbols rule))
             (list-copy (rule-move-directions rule))))
