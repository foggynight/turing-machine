(declare (unit head))

;; Make a new head equal to start, start should be an integer.
(define (make-head start)
  start)

;; Move head in the given direction by adding or subtracting one from it.
(define (move-head head direction)
  (case direction
    ('left (- head 1))
    ('right (+ head 1))))

;; Convert head into an index suitable for indexing a tape.
(define (head->index head)
  (if (negative? head)
      (- (* 2 (abs head)) 1)
      (* 2 head)))
