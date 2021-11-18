;;;; global.scm - Turing machine global parameters.

(declare (unit global))

;;; dynamic - Can be modified by CONF directives. ------------------------------

(define comment-character (make-parameter #\;))   ; SYNTAX:COMMENT CHARACTER
(define separator-string (make-parameter "->"))   ; SYNTAX:SEPARATOR STRING

(define initial-state (make-parameter "0"))       ; STATE:INITIAL STRING
(define accept-state (make-parameter "A"))        ; STATE:ACCEPT STRING
(define reject-state (make-parameter "R"))        ; STATE:REJECT STRING
(define error-state (make-parameter "E"))         ; STATE:ERROR STRING
(define extra-halt-states (make-parameter '()))   ; STATE:EXTRA-HALTS STRING*

(define blank-character (make-parameter #\_))     ; SYNTAX:BLANK CHARACTER
(define wildcard-character (make-parameter #\*))  ; SYNTAX:WILDCARD CHARACTER

(define left-character (make-parameter #\L))      ; SYNTAX:LEFT CHARACTER
(define right-character (make-parameter #\R))     ; SYNTAX:RIGHT CHARACTER
(define stay-character (make-parameter #\S))      ; SYNTAX:STAY CHARACTER

(define tape-count (make-parameter #f))           ; TAPE:COUNT NUMBER

(define deterministic (make-parameter #t))        ; MODE:DETERMINISTIC BOOLEAN

;;; static - Can't be modified by CONF directives. -----------------------------

(define move-characters (make-parameter (list (left-character)
                                              (right-character)
                                              (stay-character))))
