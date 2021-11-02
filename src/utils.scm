;;;; utils.scm - Various utility functions.

(declare (unit utils))

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
