(declare (uses transition-table))

(import (chicken process-context))

(define (main)
  (define table (parse-transition-table (car (command-line-arguments))))
  (display-transition-table table))

(main)
