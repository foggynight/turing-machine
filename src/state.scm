;;;; state.scm - Turing machine state functions

;; Make a new state which is represented by STR.
;; (make-state string) => state
(define (make-state str)
  str)

;; Determine if two states are equal, that is, determine if the strings
;; representing two states are equal.
;; (state=? state state) => boolean
(define (state=? state0 state1)
  (string=? state0 state1))
