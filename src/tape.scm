;;;; tape.scm - Turing machine tape functions.

(declare (unit tape)
         (uses global))

(import (chicken port)
        vector-lib)

(include "types.scm")

;; Make a new tape filled with the contents of STR, such that the first
;; character of STR is at head position zero, and the remaining characters trail
;; to the right.
(: make-tape (string --> tape))
(define (make-tape str)
  (let* ((str-len (string-length str))
         (tape (make-vector (* 2 str-len) (blank-character)))
         (head 0))
    (let loop ((i 0))
      (when (< i str-len)
        (set! tape (tape-write tape head (string-ref str i)))
        (set! head (move-head head (right-character)))
        (loop (+ i 1))))
    tape))

(: tape-ref (tape number --> char))
(define (tape-ref tape index)
  (vector-ref tape index))

(: tape-set! (tape number char -> void))
(define (tape-set! tape index value)
  (vector-set! tape index value))

;; Get the total length of TAPE, including blank spaces.
(: tape-length (tape --> number))
(define (tape-length tape)
  (vector-length tape))

;; Get the minimum valid head position of TAPE.
(: tape-min-head (tape --> number))
(define (tape-min-head tape)
  (- (/ (tape-length tape) 2)))

;; Get the maximum valid head position of TAPE.
(: tape-max-head (tape --> number))
(define (tape-max-head tape)
  (- (/ (tape-length tape) 2) 1))

;; Find the minimum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
(: tape-first-char (tape --> (or number null)))
(define (tape-first-char tape)
  (let loop ((h (tape-min-head tape)))
    (if (> h (tape-max-head tape))
        '()
        (if (char=? (tape-read tape h) (blank-character))
            (loop (move-head h (right-character)))
            h))))

;; Find the maximum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
(: tape-last-char (tape --> (or number null)))
(define (tape-last-char tape)
  (let loop ((h (tape-max-head tape)))
    (if (< h (tape-min-head tape))
        '()
        (if (char=? (tape-read tape h) (blank-character))
            (loop (move-head h (left-character)))
            h))))

;; Read a character from the cell at position HEAD in TAPE.
(: tape-read (tape head --> char))
(define (tape-read tape head)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      (blank-character)
      (tape-ref tape (head->index head))))

;; Write a character to the cell at position HEAD of TAPE and return TAPE. This
;; function may extend TAPE, thus the variable which contains TAPE should be set
;; to this function's return value.
(: tape-write (tape head char --> tape))
(define (tape-write tape head char)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      (let* ((old-len (tape-length tape))
             (new-len (if (zero? old-len) 1 old-len)))
        (tape-write (vector-append tape (make-vector new-len (blank-character)))
                    head char))
      (begin (tape-set! tape (head->index head) char)
             tape)))

;; Convert TAPE into its string representation, omitting any leading and
;; trailing blank cells.
(: tape->string (tape --> string))
(define (tape->string tape)
  (with-output-to-string
    (lambda ()
      (let ((first-char (tape-first-char tape))
            (last-char (tape-last-char tape)))
        (unless (null? first-char)
          (let loop ((head first-char))
            (when (<= head last-char)
              (display (tape-read tape head))
              (loop (move-head head (right-character))))))))))
