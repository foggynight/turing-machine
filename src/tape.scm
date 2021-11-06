;;;; tape.scm - Turing machine tape functions.

(declare (unit tape)
         (uses global))

(import (chicken port)
        vector-lib)

;; Make a new tape filled with the contents of STR, such that the first
;; character of STR is at head position zero, and the remaining characters trail
;; to the right.
;; (make-tape string) -> tape
(define (make-tape str)
  (let* ((str-len (string-length str))
         (tape (make-vector (* 2 str-len) (blank-character)))
         (head 0))
    (let loop ((i 0))
      (when (< i str-len)
        (set! tape (tape-write tape head (string-ref str i)))
        (set! head (move-head head 'right))
        (loop (+ i 1))))
    tape))

(define (tape-ref tape index)
  (vector-ref tape index))

(define (tape-set! tape index value)
  (vector-set! tape index value))

;; Get the total length of TAPE, including blank spaces.
;; (tape-length tape) -> integer >= 0
(define (tape-length tape)
  (vector-length tape))

;; Get the minimum valid head position of TAPE.
;; (tape-min-head tape) -> integer
(define (tape-min-head tape)
  (- (/ (tape-length tape) 2)))

;; Get the maximum valid head position of TAPE.
;; (tape-max-head tape) -> integer
(define (tape-max-head tape)
  (- (/ (tape-length tape) 2) 1))

;; Find the minimum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
;; (tape-first-char tape) -> integer | null
(define (tape-first-char tape)
  (let loop ((h (tape-min-head tape)))
    (if (> h (tape-max-head tape))
        '()
        (if (char=? (tape-read tape h) (blank-character))
            (loop (move-head h 'right))
            h))))

;; Find the maximum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
;; (tape-last-char tape) -> integer | null
(define (tape-last-char tape)
  (let loop ((h (tape-max-head tape)))
    (if (< h (tape-min-head tape))
        '()
        (if (char=? (tape-read tape h) (blank-character))
            (loop (move-head h 'left))
            h))))

;; Read a character from the cell at position HEAD in TAPE.
;; (tape-read tape head) -> character
(define (tape-read tape head)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      (blank-character)
      (tape-ref tape (head->index head))))

;; Write a character to the cell at position HEAD in TAPE and return TAPE, this
;; function may extend TAPE, thus its return value should be set to the variable
;; which contains TAPE.
;; (tape-write tape head character) -> tape
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
;; (tape->string tape) -> string
(define (tape->string tape)
  (with-output-to-string
    (lambda ()
      (let ((first-char (tape-first-char tape))
            (last-char (tape-last-char tape)))
        (unless (null? first-char)
          (let loop ((head first-char))
            (when (<= head last-char)
              (display (tape-read tape head))
              (loop (move-head head 'right)))))))))
