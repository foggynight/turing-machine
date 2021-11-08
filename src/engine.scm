;;;; engine.scm - Turing machine engine.

(declare (unit engine)
         (uses global)
         (uses head)
         (uses parser)
         (uses rule)
         (uses state)
         (uses tape))

(import (chicken format)
        (srfi 1))

(define rules)
(define state)
(define heads)
(define tapes)

;; Accessors for engine variables, to be used in the CLI.
(define (engine-state) state)
(define (engine-heads) heads)
(define (engine-tapes) tapes)

;; Initialize the engine, that is, load the program contained within
;; PROGRAM-STRING.
;; (engine-init! string) -> void
(define (engine-init! program-string)
  (set! rules (parse-program! program-string)))

;; Reset the engine by setting its state to the value of the INITIAL-STATE
;; parameter, set the positions of its heads to zero, and set its first tape to
;; the tape representation of INPUT-STR.
;; (engine-reset! string) -> void
(define (engine-reset! input-str)
  (set! state (initial-state))
  (set! heads (make-list (tape-count) 0))
  (set! tapes (cons (make-tape input-str)
                    (map make-tape (make-list (- (tape-count) 1) "")))))

;; Find the rule in RULES with a current state equal to STATE and read symbols
;; equal to READ-SYMBOLS, returns false if no rule was found.
;; (find-rule list) -> rule | false
(define (find-rule read-symbols)
  (let loop ((rules rules))
    (if (null? rules)
        #f
        (let ((rule (car rules)))
          (if (and (state=? state (rule-current-state rule))
                   (equal? read-symbols (rule-read-symbols rule)))
              rule
              (loop (cdr rules)))))))

;; Display a "no rule found" error.
;; (display-error_no-rule-found list) -> void
(define (display-error_no-rule-found read-symbols)
  (if (= (length read-symbols) 1)
      (format #t "Error: No rule found for:~%~
                  - Current state = ~A~%~
                  - Read symbol = ~A~%"
              state (car read-symbols))
      (format #t "Error: No rule found for:~%~
                  - Current state = ~A~%~
                  - Read symbols = ~A~%"
              state read-symbols)))

;; Write the characters in WRITE-SYMBOLS to the tapes in TAPES at the positions
;; of the heads in HEADS.
;; (write-tapes list) -> void
(define (write-tapes write-symbols)
  (set! tapes (map tape-write tapes heads write-symbols)))

;; Move the heads in HEADS in the directions specified by MOVE-DIRECTIONS.
;; (move-heads list) -> void
(define (move-heads move-directions)
  (define (aux head move-direction)
    (cond ((char-ci=? move-direction (left-character))
           (move-head head 'left))
          ((char-ci=? move-direction (right-character))
           (move-head head 'right))
          (else head)))
  (set! heads (map aux heads move-directions)))

;; Perform a single step of the evaluation of the program.
;; (engine-step!) -> void
(define (engine-step!)
  (let* ((read-symbols (map tape-read tapes heads))
         (rule (find-rule read-symbols)))
    (if rule
        (begin (write-tapes (rule-write-symbols rule))
               (move-heads (rule-move-directions rule))
               (set! state (rule-next-state rule)))
        (begin (display-error_no-rule-found read-symbols)
               (set! state (error-state))))))

;; Perform the entire evaluation of the program.
;; (engine-skip!) -> void
(define (engine-skip!)
  (engine-step!)
  (unless (halt-state? state)
    (engine-skip!)))
