;;;; parser.scm - Turing machine program parser

(include "utils/string.scm")

(include "global.scm")

;; Parse a transition rule from STR.
;; (parse-rule string) => rule
(define (parse-rule str)
  (define (rule-next-state rule) (list-ref rule 3))
  (define (rule-write-symbol rule) (list-ref rule 4))
  (define (rule-move-direction rule) (list-ref rule 5))
  (let ((lst (map (lambda (e)
                    (string-ref e 0))
                  (string-split str #\space))))
    (list (rule-current-state lst)
          (rule-read-symbol lst)
          (rule-next-state lst)
          (rule-write-symbol lst)
          (rule-move-direction lst))))

;; Parse the program contained within STR.
;; (parse-program string) => list
(define (parse-program str)
  (define (remove-comments lines)
    (if (null? lines)
        '()
        (let ((line (car lines)))
          (if (char=? (first-non-blank line) comment-character)
              (remove-comments (cdr lines))
              (cons line (remove-comments (cdr lines)))))))
  (let ((lines (string-split str #\newline)))
    (map parse-rule (remove-comments lines))))
