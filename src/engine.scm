;;;; engine.scm - Turing machine engine.

(include "global.scm")
(include "rule.scm")
(include "state.scm")

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
(define (evaluate-transition table current-state read-symbol)
  (if (null? table)
      '()
      (let ((rule (car table)))
        (if (and (state=? current-state (rule-current-state rule))
                 (or (char=? read-symbol (rule-read-symbol rule))
                     (char=? wildcard-character (rule-read-symbol rule))))
            (if (char=? wildcard-character (rule-write-symbol rule))
                (let ((rule (rule-copy rule)))
                  (rule-set! rule 3 read-symbol)
                  rule)
                rule)
            (evaluate-transition (cdr table) current-state read-symbol)))))
