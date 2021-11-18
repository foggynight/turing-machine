;;;; head.scm - Turing machine head functions.

(declare (unit head))

;; Move HEAD in the given DIRECTION by adding or subtracting one from it.
;; (move-head head symbol) -> head
(define (move-head head direction)
  (cond ((char=? direction (left-character)) (- head 1))
        ((char=? direction (right-character)) (+ head 1))
        (else head)))

;; Convert HEAD into an index suitable for indexing a tape.
;; (head->index head) -> integer >= 0
(define (head->index head)
  (if (negative? head)
      (- (* (abs head) 2) 1)
      (* head 2)))
