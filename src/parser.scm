;;;; parser.scm - Turing machine program parser functions

(include "utils/string.scm")

(include "global.scm")
(include "rule.scm")

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

;; Parse a program from STR, where a program is a list of transition rules.
;; (parse-program string) -> list
(define (parse-program str)
  (define (remove-comment-lines lines)
    (if (null? lines)
        '()
        (let ((line (car lines)))
          (if (char=? (first-non-blank line) comment-character)
              (remove-comment-lines (cdr lines))
              (cons line (remove-comment-lines (cdr lines)))))))
  (let ((lines (string-split str #\newline)))
    (map parse-rule (remove-comment-lines lines))))
