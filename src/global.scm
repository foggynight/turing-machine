;;;; global.scm - Turing machine global variables.

;; TODO Add CONF directives.

(declare (unit global))

;;; dynamic - Can be modified by CONF directives. ------------------------------

(define initial-state "0")              ; STATE:INITIAL
(define accept-state "A")               ; STATE:ACCEPT
(define reject-state "R")               ; STATE:REJECT
(define error-state "E")                ; STATE:ERROR
(define extra-halt-states '())          ; STATE:EXTRA-HALTS

(define comment-character #\;)          ; CHAR:COMMENT
(define blank-character #\_)            ; CHAR:BLANK
(define wildcard-character #\*)         ; CHAR:WILDCARD

(define left-character #\L)             ; CHAR:LEFT
(define right-character #\R)            ; CHAR:RIGHT
(define stay-character #\S)             ; CHAR:STAY

(define tape-count #f)                  ; TAPE:COUNT

;;; static - Can't be modified by CONF directives. -----------------------------

(define move-characters (list left-character right-character stay-character))
