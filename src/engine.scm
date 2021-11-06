;;;; engine.scm - Turing machine engine.

(declare (unit engine)
         (uses global)
         (uses head)
         (uses parser)
         (uses rule)
         (uses state)
         (uses tape))

(import (chicken format))

(define rules)
(define state)
(define head)

;; Evaluate a transition by finding and returning the rule in the transition
;; table which has a current state of CURRENT-STATE, and a read symbol of
;; READ-SYMBOL, returns null if no rule is found.
;;
;; Both the read and write symbols of a rule may be a wildcard. Assuming the
;; current state of a rule is CURRENT-STATE, if the read symbol is a wildcard,
;; the rule is selected regardless of READ-SYMBOL, and if the write symbol is a
;; wildcard, the write symbol is replaced with READ-SYMBOL.
;;
;; (evaluate-transition list string character) -> rule | null
(define (evaluate-transition rules current-state read-symbol)
  (if (null? rules)
      '()
      (let ((rule (car rules)))
        (if (and (state=? current-state (rule-current-state rule))
                 (or (char=? read-symbol (rule-read-symbol rule))
                     (char=? (wildcard-character) (rule-read-symbol rule))))
            (if (char=? (wildcard-character) (rule-write-symbol rule))
                (let ((rule (rule-copy rule)))
                  (rule-set! rule 3 read-symbol)
                  rule)
                rule)
            (evaluate-transition (cdr rules) current-state read-symbol)))))

;; TODO Replace this with signalling an error to the interface.
(define (display-error_no-rule-found read-symbol)
  (format #t "Error: No rule found for:~%~
              - Current state = ~A~%~
              - Read symbol = ~A~%"
          state read-symbol))

;; Initialize the engine, that is, load the program contained within
;; PROGRAM-STRING.
;; (engine-init! string) -> unspecified
(define (engine-init! program-string)
  (set! rules (parse-program! program-string)))

;; Reset the engine by setting its state to the value of the INITIAL-STATE
;; parameter and set the position of its head to zero.
;; (engine-reset!) -> unspecified
(define (engine-reset!)
  (set! state (initial-state))
  (set! head 0))

;; Evaluate a single transition on the input TAPE, and return the updated TAPE.
;; (engine-step! tape) -> tape
(define (engine-step! tape)
  (let* ((read-symbol (tape-read tape head))
         (rule (evaluate-transition rules state read-symbol)))
    (if (null? rule)
        (begin (display-error_no-rule-found read-symbol)
               (set! state (error-state)))
        (begin (set! tape (tape-write tape head (rule-write-symbol rule)))
               (set! head (let ((dir (rule-move-direction rule)))
                            (cond ((eqv? dir (left-character))
                                   (move-head head 'left))
                                  ((eqv? dir (right-character))
                                   (move-head head 'right))
                                  (else head))))
               (set! state (rule-next-state rule)))))
  tape)

;; Evaluate transitions using the input TAPE, skipping all steps until a halt
;; state is reached, then return the final TAPE.
;; (engine-skip! tape) -> tape
(define (engine-skip! tape)
  (if (halt-state? state)
      tape
      (engine-skip! (engine-step! tape))))

;; Get the state of the engine.
;; (engine-state) -> state
(define (engine-state)
  state)
