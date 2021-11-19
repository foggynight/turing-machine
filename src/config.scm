;;;; config.scm - Turing machine computation configurations.

(declare (unit config)
         (uses tape))

(import (srfi 1))

;; Record type representing a configuration of a computation.
(define-record config state heads tapes)

;; Get a deep copy of a config.
;; (config-copy config) -> config
(define (config-copy config)
  (make-config (config-state config)
               (list-copy (config-heads config))
               (list-copy (config-tapes config))))

;; Get a list of the symbols read from the tapes of CONFIG at the positions of
;; the heads of CONFIG.
;; (config-read-tapes config) -> list
(define (config-read-tapes config)
  (map tape-read (config-tapes config) (config-heads config)))
