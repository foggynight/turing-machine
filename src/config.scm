;;;; config.scm - Turing machine computation configurations.

(declare (unit config)
         (uses global)
         (uses tape))

(import (chicken format)
        (chicken string)
        (srfi 1))

;; Record type representing a configuration of a computation.
(define-record config state heads tapes)

;; Get a deep copy of a config.
;; (config-copy config) -> config
(define (config-copy config)
  (make-config (config-state config)
               (list-copy (config-heads config))
               (list-copy (config-tapes config))))

;; Set the state of CONFIG to ERROR-STATE.
;; (config-error! config) -> void
(define (config-error! config)
  (config-state-set! config (error-state)))

;; Get a list of the symbols read from the tapes of CONFIG at the positions of
;; the heads of CONFIG.
;; (config-read-tapes config) -> list
(define (config-read-tapes config)
  (map tape-read (config-tapes config) (config-heads config)))

;; Get the string representation of CONFIG.
;; (config->string config) -> string
(define (config->string config)
  (define state (config-state config))
  (define (state+head+tape->string head tape)
    (define str (tape->string tape))
    (define len (string-length str))
    (define index (if (= len 0) 0 (- head (tape-first-char tape))))
    (define left)
    (define right)
    (cond ((negative? index)
           (set! left "")
           (set! right (string-append (make-string (- index) (blank-character))
                                      str)))
          ((>= index len)
           (set! left (string-append str (make-string (- index len)
                                                      (blank-character))))
           (set! right (string (blank-character))))
          (else (set! left (substring str 0 index))
                (set! right (substring str index len))))
    (string-append left (format #f "(~A)" state) right))
  (string-intersperse (map state+head+tape->string
                           (config-heads config)
                           (config-tapes config))))
