;;;; global.scm - Turing machine global variables.

(declare (unit global))

(define blank-character #\_)
(define comment-character #\;)
(define wildcard-character #\*)

(define initial-state "0")
(define accept-state "A")
(define reject-state "R")
(define error-state "E")
(define extra-halt-states '())

(define left-character #\L)
(define right-character #\R)
(define stay-character #\S)
