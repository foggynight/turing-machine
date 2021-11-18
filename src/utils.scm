;;;; utils.scm - Various utility procedures.

(declare (unit utils))

(import (srfi 1))

;;; list -----------------------------------------------------------------------

;; Get a newly allocated list containing the first N elements of LST. It is an
;; error for N to be less than zero or greater than the length of LST.
;; (list-head list integer) -> list
(define (list-head lst n)
  (if (zero? n)
      '()
      (cons (car lst)
            (list-head (cdr lst) (- n 1)))))

;; Insert ELEM into LST before the element at INDEX of LST. It is an error for
;; INDEX to be outside the range [0, L] where L is the length of LST.
;; (list-insert-before list number any) -> list
(define (list-insert-before lst index elem)
  (if (zero? index)
      (cons elem lst)
      (cons (car lst)
            (list-insert-before (cdr lst) (- index 1) elem))))

;; Insert ELEM into LST after the element at INDEX of LST. It is an error for
;; INDEX to be outside the range [-1, L) where L is the length of LST.
;; (list-insert-after list number any) -> list
(define (list-insert-after lst index elem)
  (list-insert-before lst (+ index 1) elem))

;;; string ---------------------------------------------------------------------

;; Get the first character of STR, or false if STR is empty.
;; (first-character string) -> character | false
(define (first-character str)
  (if (= (string-length str) 0)
      #f
      (string-ref str 0)))

;; Get the first non-whitespace character of STR, or false if STR does not
;; contain any non-whitespace characters.
;; (first-non-whitespace string) -> character | false
(define (first-non-whitespace str)
  (let ((len (string-length str))
        (char #f))
    (let loop ((i 0))
      (unless (or char
                  (= i len))
        (let ((c (string-ref str i)))
          (unless (char-whitespace? c)
            (set! char c)))
        (loop (+ i 1))))
    char))
