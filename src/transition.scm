(declare (unit transition))

(import (chicken io)
        (chicken string))

;; Transition rule accessor functions.
(define (current-state rule) (list-ref rule 0))
(define (read-symbol rule) (list-ref rule 1))
(define (next-state rule) (list-ref rule 3))
(define (write-symbol rule) (list-ref rule 4))
(define (move-direction rule) (list-ref rule 5))

;; Parse a transition rule from a string.
(define (parse-transition-rule str)
  (let ((lst (map (lambda (e)
                    (string-ref e 0))
                  (string-split str))))
    (list (current-state lst)
          (read-symbol lst)
          (next-state lst)
          (write-symbol lst)
          (move-direction lst))))

;; Parse a transition table from a file.
(define (parse-transition-table path)
  (define (aux path table)
    (with-input-from-file path
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            (unless (char=? (string-ref line 0) #\;)
              (set! table (cons (parse-transition-rule line)
                                table)))
            (loop (read-line))))))
    table)
  (reverse (aux path '())))

;; Display a transition table, one transition rule per line.
(define (display-transition-table table)
  (unless (null? table)
    (display (car table))
    (newline)
    (display-transition-table (cdr table))))
