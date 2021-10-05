;;;; engine.scm - Turing machine engine

(include "rule.scm")
(include "state.scm")

;; Evaluate a transition by finding and returning the rule in the transition
;; table which has a current state of CURR-STATE, and a read symbol of READ-SYM,
;; returns null if no rule is found.
;; (evaluate-transition list string character) => rule | null
(define (evaluate-transition table current-state read-symbol)
  (if (null? table)
      '()
      (let ((rule (car table)))
        (if (and (state=? current-state (rule-current-state rule))
                 (char=? read-symbol (rule-read-symbol rule)))
            rule
            (evaluate-transition (cdr table) current-state read-symbol)))))
