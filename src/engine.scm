;;;; engine.scm - Turing machine engine

;; TODO Write tests, especially for utils modules.

;;; string-utils ---------------------------------------------------------------

;; Split STR on each occurence of DELIM, and return a list containing the
;; resultant substrings, omitting any empty strings.
;; (string-split string character) => list
;; e.g. (string-split " This is  a test. " #\ ) => ("This" "is" "a" "test.")
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

;;; vector-utils ---------------------------------------------------------------

;; Set the elements from the range [START, END) in DEST to the elements from the
;; range [0, END - START) in SRC.
;; (vector-range-set! vector vector integer integer) => unspecified
(define (vector-range-set! dest src start end)
  (let loop ((dest-i start)
             (src-i 0))
    (when (< dest-i end)
      (vector-set! dest dest-i (vector-ref src src-i))
      (loop (+ dest-i 1) (+ src-i 1)))))

;; Create a new vector containing the elements in VECTOR.
;; (vector-copy vector) => vector
(define (vector-copy vector)
  (let* ((len (vector-length vector))
         (new-vector (make-vector len)))
    (vector-range-set! new-vector vector 0 len)
    new-vector))

;; Append VECTORS into a new vector and return that new vector.
;; (vector-append vector*) => vector
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

;;; head -----------------------------------------------------------------------

;; Make a new head with position START.
;; (make-head integer) => head
(define (make-head start)
  start)

;; Move HEAD in the given DIRECTION by adding or subtracting one from it.
;; (move-head head symbol) => head
(define (move-head head direction)
  (case direction
    ((left) (- head 1))
    ((right) (+ head 1))))

;; Convert HEAD into an index suitable for indexing a tape.
;; (head->index head) => integer >= 0
(define (head->index head)
  (if (negative? head)
      (- (* 2 (abs head)) 1)
      (* 2 head)))

;;; tape -----------------------------------------------------------------------

;; Character used to represent a blank cell on a tape.
(define blank #\_)

;; Make a new tape filled with the contents of STR, such that the first
;; character of STR is at head position zero, and the remaining characters trail
;; to the right.
;; (make-tape string) => tape
(define (make-tape str)
  (let* ((str-len (string-length str))
         (tape (make-vector (* 2 str-len) blank))
         (head (make-head 0)))
    (let loop ((i 0))
      (when (< i str-len)
        (set! tape (tape-write tape head (string-ref str i)))
        (set! head (move-head head 'right))
        (loop (+ i 1))))
    tape))

;; Get the total length of TAPE, including blank spaces.
;; (tape-length tape) => integer >= 0
(define (tape-length tape)
  (vector-length tape))

(define (tape-ref tape index)
  (vector-ref tape index))

(define (tape-set! tape index value)
  (vector-set! tape index value))

;; Get the minimum valid head position of TAPE.
;; (tape-min-head tape) => integer
(define (tape-min-head tape)
  (- (/ (tape-length tape) 2)))

;; Get the maximum valid head position of TAPE.
;; (tape-max-head tape) => integer
(define (tape-max-head tape)
  (- (/ (tape-length tape) 2) 1))

;; Find the minimum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
;; (tape-first-char tape) => integer | null
(define (tape-first-char tape)
  (let loop ((h (make-head (tape-min-head tape))))
    (if (> h (tape-max-head tape))
        '()
        (if (char=? (tape-read tape h) blank)
            (loop (move-head h 'right))
            h))))

;; Find the maximum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
;; (tape-last-char tape) => integer | null
(define (tape-last-char tape)
  (let loop ((h (make-head (tape-max-head tape))))
    (if (< h (tape-min-head tape))
        '()
        (if (char=? (tape-read tape h) blank)
            (loop (move-head h 'left))
            h))))

;; Read a character from the cell at position HEAD in TAPE.
;; (tape-read tape head) => character
(define (tape-read tape head)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      blank
      (tape-ref tape (head->index head))))

;; Write a character to the cell at position HEAD in TAPE and return TAPE, this
;; function may extend TAPE, thus its return value should be set to the variable
;; which contains TAPE.
;; (tape-write tape head character) => tape
(define (tape-write tape head char)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      (let* ((old-len (tape-length tape))
             (new-len (if (zero? old-len) 1 old-len)))
        (tape-write (vector-append tape (make-vector new-len blank))
                    head char))
      (begin (tape-set! tape (head->index head) char)
             tape)))

;; Display TAPE, omitting any leading and trailing blank cells.
;; (display-tape tape) => unspecified
(define (display-tape tape)
  (let ((first-char (tape-first-char tape))
        (last-char (tape-last-char tape)))
    (unless (null? first-char)
      (let loop ((head (make-head first-char)))
        (when (<= head last-char)
          (display (tape-read tape head))
          (loop (move-head head 'right)))))))

;;; state ----------------------------------------------------------------------

;; Make a new state which is represented by CHAR.
;; (make-state character) => state
(define (make-state char)
  char)

;;; transition -----------------------------------------------------------------

;; Transition rule accessor functions.
(define (current-state rule) (list-ref rule 0))
(define (read-symbol rule) (list-ref rule 1))
(define (next-state rule) (list-ref rule 2))
(define (write-symbol rule) (list-ref rule 3))
(define (move-direction rule) (list-ref rule 4))

;; Parse a transition rule from STR.
;; (parse-rule string) => rule
(define (parse-rule str)
  (define (next-state rule) (list-ref rule 3))
  (define (write-symbol rule) (list-ref rule 4))
  (define (move-direction rule) (list-ref rule 5))
  (let ((lst (map (lambda (e)
                    (string-ref e 0))
                  (string-split str #\ ))))
    (list (current-state lst)
          (read-symbol lst)
          (next-state lst)
          (write-symbol lst)
          (move-direction lst))))

;; Evaluate a transition by returning the desired rule in the transition table.
;; (evaluate-transition list character character) => rule
(define (evaluate-transition table curr-state read-sym)
  (let ((rule (car table)))
    (if (and (char=? curr-state (current-state rule))
             (char=? read-sym (read-symbol rule)))
        rule
        (evaluate-transition (cdr table) curr-state read-sym))))
