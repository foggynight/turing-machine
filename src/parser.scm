;;;; parser.scm - Turing machine program parser.

(declare (unit parser)
         (uses global)
         (uses utils))

(import (chicken string))

;; Number of the current line being parsed, first line is number one.
(define line-number 0)

;; Signal an error with a message containing LINE-NUMBER and MSG.
;; (parser-error string) -> void
(define (parser-error msg)
  (error (string-append "Line " (number->string line-number) ": " msg)))

;; Parse a CONF directive and configure the Turing machine.
;; (parse-conf! string) -> void
(define (parse-conf! line)
  (define split (string-split line))
  (when (< (length split) 2)
    (parser-error "Invalid CONF directive"))
  (cond ((string-ci=? (car split) "STATE:INITIAL")
         (set! initial-state (cadr split)))
        ((string-ci=? (car split) "STATE:ACCEPT")
         (set! accept-state (cadr split)))
        ((string-ci=? (car split) "STATE:REJECT")
         (set! reject-state (cadr split)))
        ((string-ci=? (car split) "STATE:ERROR")
         (set! error-state (cadr split)))
        ((string-ci=? (car split) "STATE:EXTRA-HALTS")
         (set! extra-halt-states (append extra-halt-states (cdr split))))
        ((string-ci=? (car split) "TAPE:COUNT")
         (set! tape-count (string->number (cadr split))))
        (else (parser-error "Invalid CONF directive"))))

;; Get the number of tapes used in the rule represented by CURR and NEXT.
;; (get-tape-count list list) -> integer >= 0
(define (get-tape-count curr next)
  (define (get-read-count)
    (- (length curr) 1))
  (define (get-write-count)
    (inexact->exact (truncate (/ (- (length next) 1) 2))))
  (let ((count (get-read-count)))
    (unless (= count (get-write-count))
      (parser-error "Invalid rule: Read/write counts not equal"))
    count))

;; Parse and return the transition rule contained within CURR and NEXT, check if
;; the rule uses the right number of tapes, check if the move characters used in
;; the rule are valid, and set the value of TAPE-COUNT if necessary.
;; (parse-rule! list list) -> rule
(define (parse-rule! curr next)
  (define (validate-read/write-string str)
    (= (string-length str) 1))
  (if tape-count
      (unless (= (get-tape-count curr next) tape-count)
        (parser-error "Invalid rule: Invalid tape count"))
      (set! tape-count (get-tape-count curr next)))
  (unless (not (memq #f (map (lambda (e)
                               (member (string-ref e 0) move-characters))
                             (list-tail next (+ tape-count 1)))))
    (parser-error "Invalid rule: Invalid move character"))
  (list (cons (car curr) (map first-character (cdr curr)))
        (cons (car next) (map first-character (cdr next)))))

;; Parse and return the program contained within STR, where a program is a list
;; of transition rules. This function also configures the Turing machine if the
;; program contains a CONF section.
;; (parse-program! string) -> list
(define (parse-program! str)
  (define (line-comment? line)
    (char=? (first-non-whitespace line) comment-character))
  (define (line-conf-begin? line)
    (string-ci=? (car (string-split line)) "CONF:BEGIN"))
  (define (line-conf-end? line)
    (string-ci=? (car (string-split line)) "CONF:END"))
  (define in-conf #f)
  (define rules '())
  (set! line-number 0)
  (let loop ((lines (string-split str (string #\newline))))
    (unless (null? lines)
      (set! line-number (+ line-number 1))
      (let ((line (car lines)))
        (cond ((line-comment? line))
              ((line-conf-begin? line)
               (if in-conf
                   (parser-error "Invalid CONF:BEGIN directive")
                   (set! in-conf #t)))
              ((line-conf-end? line)
               (if in-conf
                   (set! in-conf #f)
                   (parser-error "Invalid CONF:END directive")))
              (in-conf (parse-conf! line))
              (else
               (let ((split (string-split line)))
                 (unless (member "->" split)
                   (parser-error "Invalid rule: Separator not found"))
                 (let ((curr (let loop ((lst split))
                               (if (string=? (car lst) "->")
                                   '()
                                   (cons (car lst) (loop (cdr lst))))))
                       (next (cdr (member "->" split))))
                   (set! rules (cons (parse-rule! curr next) rules)))))))
      (loop (cdr lines))))
  (reverse rules))
