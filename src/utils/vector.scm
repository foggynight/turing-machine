;;;; vector.scm - Vector utility functions.

;; Set the elements from the range [START, END) in DEST to the elements from the
;; range [0, END - START) in SRC.
;; (vector-range-set! vector vector integer integer) -> unspecified
(define (vector-range-set! dest src start end)
  (let loop ((dest-i start)
             (src-i 0))
    (when (< dest-i end)
      (vector-set! dest dest-i (vector-ref src src-i))
      (loop (+ dest-i 1) (+ src-i 1)))))

;; Create a new vector containing the elements in VECTOR.
;; (vector-copy vector) -> vector
(define (vector-copy vector)
  (let* ((len (vector-length vector))
         (new-vector (make-vector len)))
    (vector-range-set! new-vector vector 0 len)
    new-vector))

;; Append VECTORS into a new vector and return that new vector.
;; (vector-append vector*) -> vector
(define (vector-append . vectors)
  (define (compute-length vectors/list)
    (apply + (map vector-length vectors/list)))
  (cond ((null? vectors) (make-vector 0))
        ((null? (cdr vectors)) (vector-copy (car vectors)))
        (else (let ((new-vector (make-vector (compute-length vectors)))
                    (new-vector-index 0))
                (let loop ((vecs vectors))
                  (unless (null? vecs)
                    (let* ((vec (car vecs))
                           (next-new-vector-index (+ new-vector-index
                                                     (vector-length vec))))
                      (vector-range-set! new-vector
                                         vec
                                         new-vector-index
                                         next-new-vector-index)
                      (set! new-vector-index next-new-vector-index))
                    (loop (cdr vecs))))
                new-vector))))
