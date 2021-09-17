(declare (unit tape))

(define (tape-get tape index)
  (list-ref tape index))

(define (tape-set! tape index value)
  (set! (list-ref tape index) value))

(define tape-ref (getter-with-setter tape-get tape-set!))

(define (head->index head)
  (if (negative? head)
      (- (* (abs head) 2) 1)
      (* head 2)))

;; Read a character from a tape at the location of a head.
(define (read-tape tape head)
  (tape-ref tape (head->index head)))

;; Write a character to a tape at the location of a head.
(define (write-tape tape head)
  (tape-ref tape (head->index head)))
