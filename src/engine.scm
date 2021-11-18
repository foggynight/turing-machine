;;;; engine.scm - Turing machine engine.

(declare (unit engine)
         (uses config)
         (uses global)
         (uses head)
         (uses parser)
         (uses rule)
         (uses state)
         (uses tape))

(import (chicken format)
        (srfi 1))

(define rules)
(define configs)
(define final-state)
(define final-tapes)

(define (engine-configs) configs)
(define (engine-final-state) final-state)
(define (engine-final-tapes) final-tapes)

;; Initialize the engine, that is, load the program contained within
;; PROGRAM-STRING.
;; (engine-init! string) -> void
(define (engine-init! program-string)
  (set! rules (parse-program! program-string)))

;; Reset the engine by setting CONFIGS to a list containing only the initial
;; configuration of the Turing machine.
;; (engine-reset! string) -> void
(define (engine-reset! input-str)
  (set! configs
    (list (make-config (initial-state)
                       (make-list (tape-count) 0)
                       (cons (make-tape input-str)
                             (map make-tape
                                  (make-list (- (tape-count) 1) "")))))))

;; Get a new rule containing the elements of RULE, with any wildcards in the
;; read/write symbols of RULE replaced with READ-SYMBOLS.
;; (replace-wildcards rule list) -> rule
(define (replace-wildcards rule read-symbols)
  (define (replace-wildcard rule-symbol given-symbol)
    (if (char=? rule-symbol (wildcard-character))
        given-symbol
        rule-symbol))
  (set! rule (rule-copy rule))
  (rule-read-symbols-set! rule (map replace-wildcard
                                    (rule-read-symbols rule)
                                    read-symbols))
  (rule-write-symbols-set! rule (map replace-wildcard
                                     (rule-write-symbols rule)
                                     read-symbols))
  rule)

;; Find the rule in RULES with a current state equal to STATE and read symbols
;; equal to READ-SYMBOLS, returns false if no rule was found.
;; (find-rule state list) -> rule | false
(define (find-rule state read-symbols)
  (let loop ((rules rules))
    (if (null? rules)
        #f
        (let ((rule (replace-wildcards (car rules) read-symbols)))
          (if (and (state=? state (rule-current-state rule))
                   (equal? read-symbols (rule-read-symbols rule)))
              rule
              (loop (cdr rules)))))))

;; Display a "no rule found" error.
;; (display-error_no-rule-found config list) -> void
(define (display-error_no-rule-found config read-symbols)
  (if (= (length read-symbols) 1)
      (format #t "Error: No rule found for:~%~
                  - Current state = ~A~%~
                  - Read symbol = ~A~%"
              (config-state config)
              (car read-symbols))
      (format #t "Error: No rule found for:~%~
                  - Current state = ~A~%~
                  - Read symbols = ~A~%"
              (config-state config)
              read-symbols)))

;; Write the characters in WRITE-SYMBOLS to the tapes in CONFIG at the positions
;; of the heads in CONFIG.
;; (write-tapes! config list) -> void
(define (write-tapes! config write-symbols)
  (config-tapes-set! config (map tape-write
                                 (config-tapes config)
                                 (config-heads config)
                                 write-symbols)))

;; Move the heads in CONFIG in the directions specified by MOVE-DIRECTIONS.
;; (move-heads! config list) -> void
(define (move-heads! config move-directions)
  (config-heads-set! config (map move-head
                                 (config-heads config)
                                 move-directions)))

;; Perform a single step of the evaluation of the program. It is an error for
;; this function to be called when CONFIGS is empty.
;; (engine-step!) -> void
(define (engine-step!)
  (define config (car configs))
  (set! configs (cdr configs))
  (let* ((state (config-state config))
         (read-symbols (map tape-read
                            (config-tapes config)
                            (config-heads config)))
         (rule (find-rule state read-symbols)))
    (if rule
        (begin (write-tapes! config (rule-write-symbols rule))
               (move-heads! config (rule-move-directions rule))
               (config-state-set! config (rule-next-state rule)))
        (begin (display-error_no-rule-found config read-symbols)
               (config-state-set! config (error-state)))))
  (if (halt-state? (config-state config))
      (begin (set! final-state (config-state config))
             (set! final-tapes (config-tapes config)))
      (set! configs (cons config configs))))

;; Perform the entire evaluation of the program.
;; (engine-skip!) -> void
(define (engine-skip!)
  (engine-step!)
  (unless (null? configs)
    (engine-skip!)))
