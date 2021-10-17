;;;; global.scm - Turing machine global variables.

(define comment-character #\;)

(define blank-character #\_)
(define wildcard-character #\*)

(define initial-state "0")
(define accept-state "A")
(define reject-state "R")
(define error-state "E")
(define extra-halt-states '())

(define left-character #\L)
(define right-character #\R)
(define stay-character #\S)
