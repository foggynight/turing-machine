(declare (unit tape))

(define (tape-get tape index)
  (list-ref tape index))

(define (tape-set! tape index value)
  (set! (list-ref tape index) value))

(define tape-ref (getter-with-setter tape-get tape-set!))
