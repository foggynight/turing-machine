;;;; utils.scm - Various utility functions.

(declare (unit utils))

;;; list -----------------------------------------------------------------------

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

;;; string ---------------------------------------------------------------------

;; Return the first non-whitespace character of STR, or false if STR does not
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
