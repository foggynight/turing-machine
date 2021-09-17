(declare (unit head)
         (uses tape))

(define (move-left head)
  (- head 1))

(define (move-right head)
  (+ head 1))

(define (head->index head)
  (if (negative? head)
      (- (* (abs head) 2) 1)
      (* head 2)))

;; Read a character from a tape at the location of a head.
(define (head-read head tape)
  (tape-ref tape (head->index head)))

;; Write a character to a tape at the location of a head.
(define (head-write head tape)
  (tape-ref tape (head->index head)))
