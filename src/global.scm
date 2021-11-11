;;;; global.scm - Turing machine global parameters.

(declare (unit global))

;;; dynamic - Can be modified by CONF directives. ------------------------------

(define initial-state (make-parameter "0"))       ; STATE:INITIAL
(define accept-state (make-parameter "A"))        ; STATE:ACCEPT
(define reject-state (make-parameter "R"))        ; STATE:REJECT
(define error-state (make-parameter "E"))         ; STATE:ERROR
(define extra-halt-states (make-parameter '()))   ; STATE:EXTRA-HALTS

(define comment-character (make-parameter #\;))   ; SYNTAX:COMMENT
(define blank-character (make-parameter #\_))     ; SYNTAX:BLANK
(define wildcard-character (make-parameter #\*))  ; SYNTAX:WILDCARD

(define left-character (make-parameter #\L))      ; SYNTAX:LEFT
(define right-character (make-parameter #\R))     ; SYNTAX:RIGHT
(define stay-character (make-parameter #\S))      ; SYNTAX:STAY

(define tape-count (make-parameter #f))           ; TAPE:COUNT

;;; static - Can't be modified by CONF directives. -----------------------------

(define move-characters (make-parameter (list (left-character)
                                              (right-character)
                                              (stay-character))))
