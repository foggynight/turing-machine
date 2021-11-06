;;;; state.scm - Turing machine state functions.

(declare (unit state))

;; Determine if two states are equal.
;; (state=? state state) -> boolean
(define (state=? state0 state1)
  (string=? state0 state1))

;; Determine if STATE is a halt state.
;; (halt-state? state) -> boolean
(define (halt-state? state)
  (or (state=? state (accept-state))
      (state=? state (reject-state))
      (state=? state (error-state))
      (member state (extra-halt-states))))
