;;;; string.scm - String utility functions

;; Split STR on each occurence of DELIM, and return a list containing the
;; resultant substrings, omitting any empty strings.
;; (string-split string character) => list
;; e.g. (string-split " This is  a test. " #\space) => ("This" "is" "a" "test.")
(define (string-split str delim)
  (let ((capturing #f)
        (start 0)
        (end 0)
        (str-len (string-length str))
        (str-list '()))
    (let loop ((str-i 0))
      (if (< str-i str-len)
          (begin (if capturing
                     (begin (set! end str-i)
                            (when (char=? (string-ref str end) delim)
                              (set! str-list (cons (substring str start end)
                                                   str-list))
                              (set! start end)
                              (set! capturing #f)))
                     (begin (set! start str-i)
                            (unless (char=? (string-ref str start) delim)
                              (set! end start)
                              (set! capturing #t))))
                 (loop (+ str-i 1)))
          (begin (when capturing
                   (set! str-list (cons (substring str start str-len)
                                        str-list)))
                 (reverse str-list))))))

;; Return the first non-blank character of STR, or null if STR does not contain
;; any non-blank characters. Blank characters are spaces, tabs, and newlines.
;; (first-non-blank string) => character | null
(define (first-non-blank str)
  (let ((len (string-length str))
        (char '()))
    (unless (= len 0)
      (let loop ((i 0))
        (unless (or (not (null? char))
                    (= i len))
          (let ((c (string-ref str i)))
            (case c
              ((#\space #\tab #\newline))
              (else (set! char c))))
          (loop (+ i 1)))))
    char))
