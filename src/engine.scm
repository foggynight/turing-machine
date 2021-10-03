;;;; engine.scm - Turing machine engine

;;; setup ----------------------------------------------------------------------

;; Character used to represent a blank cell on a tape.
(define blank-character)

;; Setup the engine by assigning BLANK-CHAR to the blank-character variable.
;; (setup-engine character) => unspecified
(define (setup-engine blank-char)
  (set! blank-character blank-char))

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

;; Make a new tape filled with the contents of STR, such that the first
;; character of STR is at head position zero, and the remaining characters trail
;; to the right.
;; (make-tape string) => tape
(define (make-tape str)
  (let* ((str-len (string-length str))
         (tape (make-vector (* 2 str-len) blank-character))
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
        (if (char=? (tape-read tape h) blank-character)
            (loop (move-head h 'right))
            h))))

;; Find the maximum head position of a non-blank cell in TAPE, returns null if
;; there is no non-blank cell found.
;; (tape-last-char tape) => integer | null
(define (tape-last-char tape)
  (let loop ((h (make-head (tape-max-head tape))))
    (if (< h (tape-min-head tape))
        '()
        (if (char=? (tape-read tape h) blank-character)
            (loop (move-head h 'left))
            h))))

;; Read a character from the cell at position HEAD in TAPE.
;; (tape-read tape head) => character
(define (tape-read tape head)
  (if (or (< head (tape-min-head tape))
          (> head (tape-max-head tape)))
      blank-character
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
        (tape-write (vector-append tape (make-vector new-len blank-character))
                    head char))
      (begin (tape-set! tape (head->index head) char)
             tape)))

;;; state ----------------------------------------------------------------------

;; Make a new state which is represented by CHAR.
;; (make-state character) => state
(define (make-state char)
  char)

;;; transition -----------------------------------------------------------------

;; Transition rule accessor functions.
(define (rule-current-state rule) (list-ref rule 0))
(define (rule-read-symbol rule) (list-ref rule 1))
(define (rule-next-state rule) (list-ref rule 2))
(define (rule-write-symbol rule) (list-ref rule 3))
(define (rule-move-direction rule) (list-ref rule 4))

;; Parse a transition rule from STR.
;; (parse-rule string) => rule
(define (parse-rule str)
  (define (rule-next-state rule) (list-ref rule 3))
  (define (rule-write-symbol rule) (list-ref rule 4))
  (define (rule-move-direction rule) (list-ref rule 5))
  (let ((lst (map (lambda (e)
                    (string-ref e 0))
                  (string-split str #\ ))))
    (list (rule-current-state lst)
          (rule-read-symbol lst)
          (rule-next-state lst)
          (rule-write-symbol lst)
          (rule-move-direction lst))))

;; Evaluate a transition by finding and returning the rule in the transition
;; table which has a current state of CURR-STATE, and a read symbol of READ-SYM,
;; returns null if no rule is found.
;; (evaluate-transition list character character) => rule | null
(define (evaluate-transition table current-state read-symbol)
  (if (null? table)
      '()
      (let ((rule (car table)))
        (if (and (char=? current-state (rule-current-state rule))
                 (char=? read-symbol (rule-read-symbol rule)))
            rule
            (evaluate-transition (cdr table) current-state read-symbol)))))
