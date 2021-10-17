;;;; list.scm - List utility functions.

;; Get a copy of LST.
;; (list-copy list) -> list
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (list-copy (cdr lst)))))

;; Get a sublist obtained by omitting all but the first K elements of LST. It is
;; an error for LST to contain less than K elements.
;; (list-head list integer) -> list
(define (list-head lst k)
  (if (positive? k)
      (cons (car lst) (list-head (cdr lst) (- k 1)))
      '()))

;; Get a list containing the elements from the range [START, END) of LST. START
;; and END must be non-negative integers. It is an error for START to be greater
;; than L-1 or for END to be greater than L, where L is the length of LST.
;; (sublist list integer integer) -> list
(define (sublist lst start end)
  (list-head (list-tail lst start) (- end start)))
