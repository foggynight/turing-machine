;;;; parser.scm - Turing machine program parser functions.

(include "utils/string.scm")

(include "global.scm")

;; Remove comments from LINES, where a comment is a line whose first
;; non-whitespace character is a comment character.
;; (remove-comments list) -> list
(define (remove-comments lines)
  (if (null? lines)
      '()
      (let ((line (car lines)))
        (if (char=? (first-non-blank line) comment-character)
            (remove-comments (cdr lines))
            (cons line (remove-comments (cdr lines)))))))

;; Parse a CONF directive and configure the Turing machine.
;; (parse-conf! string) -> unspecified
(define (parse-conf! line)
  (let ((split-str (string-split line #\space)))
    (when (< (length split-str) 2)
      (error "parse-conf!: Invalid CONF directive"))
    (cond ((string-ci=? (car split-str) "STATE:INITIAL")
           (set! initial-state (cadr split-str)))
          ((string-ci=? (car split-str) "STATE:ACCEPT")
           (set! accept-state (cadr split-str)))
          ((string-ci=? (car split-str) "STATE:REJECT")
           (set! reject-state (cadr split-str)))
          ((string-ci=? (car split-str) "STATE:ERROR")
           (set! error-state (cadr split-str)))
          ((string-ci=? (car split-str) "STATE:EXTRA-HALTS")
           (set! extra-halt-states (append extra-halt-states (cdr split-str))))
          (else (error "parse-conf!: Invalid CONF directive")))))

;; Parse and remove the CONF section of LINES, should it contain one, and return
;; LINES. Each line in the CONF section is passed to PARSE-CONF! to configure
;; the Turing machine.
;; (parse-and-remove-conf! list) -> list
(define (parse-and-remove-conf! lines)
  (define (find-line test)
    (let loop ((i 0)
               (lines lines))
      (if (null? lines)
          -1
          (if (test (car lines))
              i
              (loop (+ i 1) (cdr lines))))))
  (define (conf-begin? line)
    (let ((split-str (string-split line #\space)))
      (string-ci=? line "CONF:BEGIN")))
  (define (conf-end? line)
    (let ((split-str (string-split line #\space)))
      (string-ci=? line "CONF:END")))
  (define (remove-conf start end)
    (define (aux lines i)
      (if (null? lines)
          '()
          (if (and (>= i start)
                   (<= i end))
              (aux (cdr lines) (+ i 1))
              (cons (car lines) (aux (cdr lines) (+ i 1))))))
    (aux lines 0))
  (define conf-start (find-line conf-begin?))
  (define conf-end (find-line conf-end?))
  (unless (= conf-start conf-end -1)
    (when (or (= conf-start -1)
              (= conf-end -1)
              (>= conf-start conf-end))
      (error "parse-and-remove-conf!: Invalid CONF section")))
  (for-each parse-conf! (sublist lines (+ conf-start 1) conf-end))
  (remove-conf conf-start conf-end))

;; Parse a transition rule from STR.
;; (parse-rule string) -> rule
;; e.g. (parse-rule "0 0 -> 0 0 R") -> ("0" #\0 "0" #\0 #\R)
(define (parse-rule str)
  (let ((split-str (string-split str #\space)))
    (list (list-ref split-str 0)
          (string-ref (list-ref split-str 1) 0)
          (list-ref split-str 3)
          (string-ref (list-ref split-str 4) 0)
          (string-ref (list-ref split-str 5) 0))))

;; Parse a program from STR, where a program is a list of transition rules. Also
;; configures the Turing machine using the CONF section of STR, should it
;; contain one.
;; (parse-program string) -> list
(define (parse-program! str)
  (let ((lines (string-split str #\newline)))
    (map parse-rule (parse-and-remove-conf! (remove-comments lines)))))
