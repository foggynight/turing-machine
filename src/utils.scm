;;;; utils.scm - Various utility functions.

(declare (unit utils))

(import (srfi 1))

;;; list -----------------------------------------------------------------------

;; Get a newly allocated list containing the first N elements of LST. It is an
;; error for N to be less than zero or greater than the length of LST.
(: list-head (list number --> list))
(define (list-head lst n)
  (if (zero? n)
      '()
      (cons (car lst)
            (list-head (cdr lst) (- n 1)))))

;;; string ---------------------------------------------------------------------

;; Get the first character of STR, or false if STR is empty.
(: first-character (string --> (or char false)))
(define (first-character str)
  (if (= (string-length str) 0)
      #f
      (string-ref str 0)))

;; Get the first non-whitespace character of STR, or false if STR does not
;; contain any non-whitespace characters.
(: first-non-whitespace (string --> (or char false)))
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
