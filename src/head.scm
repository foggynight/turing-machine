;;;; head.scm - Turing machine head functions.

;; Make a new head with position START.
;; (make-head integer) -> head
(define (make-head start)
  start)

;; Move HEAD in the given DIRECTION by adding or subtracting one from it.
;; (move-head head symbol) -> head
(define (move-head head direction)
  (case direction
    ((left) (- head 1))
    ((right) (+ head 1))))

;; Convert HEAD into an index suitable for indexing a tape.
;; (head->index head) -> integer >= 0
(define (head->index head)
  (if (negative? head)
      (- (* 2 (abs head)) 1)
      (* 2 head)))
