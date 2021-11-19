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

;; Index of the next CONFIG to run within CONFIGS, this is only used in
;; nondeterministic mode.
(define run-index)

(define (engine-configs) configs)

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
                                  (make-list (- (tape-count) 1) ""))))))
  (unless (deterministic)
    (set! run-index 0)))

;; Determine if the engine has completed evaluating the program.
;; (engine-done?) -> boolean
(define (engine-done?)
  (if (deterministic)
      (halt-state? (config-state (car configs)))
      (not (member #f (map (lambda (e)
                             (halt-state? (config-state e)))
                           configs)))))

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

;; Get the first rule in RULES with a current state equal to the state of CONFIG
;; and read symbols equal to the read symbols of CONFIG, replaces any wildcards
;; in the rule with the read symbols of CONFIG.
;; (find-rule config) -> rule | false
(define (find-rule config)
  (define state (config-state config))
  (define read-symbols (config-read-tapes config))
  (let loop ((rules rules))
    (if (null? rules)
        #f
        (let ((rule (replace-wildcards (car rules) read-symbols)))
          (if (and (state=? state (rule-current-state rule))
                   (equal? read-symbols (rule-read-symbols rule)))
              rule
              (loop (cdr rules)))))))

;; Get a list of the rules in RULES with a current state equal to the state of
;; CONFIG and read symbols equal to the read symbols of CONFIG, does not replace
;; any wildcards in the rule with the read symbols of CONFIG.
;; (find-rules config) -> list
(define (find-rules config)
  (define state (config-state config))
  (define read-symbols (config-read-tapes config))
  (let loop ((rules rules))
    (if (null? rules)
        '()
        (let ((rule (car rules)))
          (if (and (state=? state (rule-current-state rule))
                   (equal? read-symbols (rule-read-symbols rule)))
              (cons rule (loop (cdr rules)))
              (loop (cdr rules)))))))

;; Display a "no rule found" error.
;; (display-error_no-rule-found config) -> void
(define (display-error_no-rule-found config)
  (define read-symbols (config-read-tapes config))
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

;; TODO Write a comment for this procedure.
(define (update-config! config rule)
  (write-tapes! config (rule-write-symbols rule))
  (move-heads! config (rule-move-directions rule))
  (config-state-set! config (rule-next-state rule)))

;; Perform a single step of the evaluation of the program.
;; TODO Expand on the comment above.
;; (engine-step!) -> void
(define (engine-step!)
  (define (aux-deterministic)
    (define config (car configs))
    (let ((rule (find-rule config)))
      (if rule
          (update-config! config rule)
          ;; TODO Replace this with updated error function.
          (begin (display-error_no-rule-found config)
                 (config-state-set! config (error-state))))))
  (define (aux-nondeterministic)
    (define config (list-ref configs run-index))
    (let ((rules (find-rules config)))
      (if (null? rules)
          (begin (display-error_no-rule-found config)
                 (config-state-set! config (error-state)))
          (begin (let loop ((r (reverse (cdr rules))))
                   (unless (null? r)
                     (let ((c (config-copy config)))
                       (update-config! c (car r))
                       (set! configs
                         (list-insert-after configs run-index c)))
                     (loop (cdr r))))
                 (update-config! config (car rules)))))
    (when (halt-state? (config-state config))
      (set! run-index (+ run-index 1))))
  (if (deterministic)
      (aux-deterministic)
      (aux-nondeterministic)))

;; Perform the entire evaluation of the program.
;; (engine-skip!) -> void
(define (engine-skip!)
  (engine-step!)
  (unless (engine-done?)
    (engine-skip!)))
