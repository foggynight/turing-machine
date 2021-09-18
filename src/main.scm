(declare (uses tape)
         (uses transition))

(import (chicken io)
        (chicken process-context))

(define (main)
  (define table (parse-transition-table (car (command-line-arguments))))
  (display-transition-table table)
  (let loop ((tape (make-tape (read-line))))
    (display "=> ")
    (display-tape tape)
    (newline)
    (loop (make-tape (read-line)))))

(main)
