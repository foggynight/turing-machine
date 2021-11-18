;;;; config.scm - Turing machine computation configurations.

(declare (unit config))

(import (srfi 1))

;; Record type representing a configuration of a computation.
(define-record config state heads tapes)

;; Get a deep copy of a config.
;; (config-copy config) -> config
(define (config-copy config)
  (make-config state
               (list-copy heads)
               (list-copy tapes)))
