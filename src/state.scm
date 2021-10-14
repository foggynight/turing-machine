;;;; state.scm - Turing machine state functions.

;; Make a new state which is represented by STR.
;; (make-state string) -> state
(define (make-state str)
  str)

;; Determine if two states are equal.
;; (state=? state state) -> boolean
(define (state=? state0 state1)
  (string=? state0 state1))

;; Determine if STATE is a halt state, that is, accept, reject, or error.
;; (halt-state? state) -> boolean
(define (halt-state? state)
  (or (state=? state accept-state)
      (state=? state reject-state)
      (state=? state error-state)))
