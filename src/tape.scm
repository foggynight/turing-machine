(declare (unit tape)
         (uses head))

;; Character used to represent a blank cell on a tape.
(define blank #\_)

;; Make a new tape filled with the contents of str, such that the first
;; character of str is at head position zero, and the remaining characters trail
;; to the right.
(define (make-tape str)
  (let* ((str-len (string-length str))
         (tape (make-vector (* 2 str-len) blank))
         (head (make-head 0)))
    (let loop ((i 0))
      (when (< i str-len)
        (tape-write! tape head (string-ref str i))
        (set! head (move-head head 'right))
        (loop (+ i 1))))
    tape))

;; Get the total length of a tape, including blank spaces.
(define (tape-length tape)
  (vector-length tape))

(define (tape-get tape index)
  (vector-ref tape index))

(define (tape-set! tape index value)
  (set! (vector-ref tape index) value))

(define tape-ref (getter-with-setter tape-get tape-set!))

;; Get the minimum valid head position of a tape.
(define (tape-min-head tape)
  (- (/ (tape-length tape) 2)))

;; Get the maximum valid head position of a tape.
(define (tape-max-head tape)
  (- (/ (tape-length tape) 2) 1))

;; Find the minimum head position of a non-blank cell in a tape, returns null if
;; there is no non-blank cell found.
(define (tape-first-char tape)
  (let loop ((h (make-head (tape-min-head tape))))
    (if (> h (tape-max-head tape))
        '()
        (if (char=? (tape-read tape h) blank)
            (loop (move-head h 'right))
            h))))

;; Find the maximum head position of a non-blank cell in a tape, returns null if
;; there is no non-blank cell found.
(define (tape-last-char tape)
  (let loop ((h (make-head (tape-max-head tape))))
    (if (< h (tape-min-head tape))
        '()
        (if (char=? (tape-read tape h) blank)
            (loop (move-head h 'left))
            h))))

;; TODO Handle reading and writing out of bounds of the tape by checking against
;; the bounds when reading, and expanding the tape as needed when writing.

;; Read a character from the cell at position head in tape.
(define (tape-read tape head)
  (tape-ref tape (head->index head)))

;; Write a character to the cell at position head in tape.
(define (tape-write! tape head char)
  (set! (tape-ref tape (head->index head)) char))

;; Display a tape, omitting any leading and trailing blank cells.
(define (display-tape tape)
  (let ((first-char (tape-first-char tape))
        (last-char (tape-last-char tape)))
    (unless (null? first-char)
      (let loop ((head first-char))
        (when (<= head last-char)
          (display (tape-read tape head))
          (loop (move-head head 'right)))))))
