;;;; engine.scm - Turing machine engine.

(declare (unit engine)
         (uses config)
         (uses global)
         (uses head)
         (uses parser)
         (uses rule)
         (uses state)
         (uses tape)
         (uses utils))

(import (srfi 1))

(define rules)   ; List of transition rules representing a program.
(define configs) ; Tree of configurations representing a computation.

(define (engine-configs) configs)

;; Initialize the engine, that is, load the program contained within
;; PROGRAM-STRING.
;; (engine-init! string) -> void
(define (engine-init! program-string)
  (set! rules (parse-program! program-string)))

;; Reset the engine by setting CONFIGS to a tree containing only the initial
;; configuration of the Turing machine.
;; (engine-reset! string) -> void
(define (engine-reset! input-str)
  (set! configs
    (make-tree (make-config (initial-state)
                            (make-list (tape-count) 0)
                            (cons (make-tape input-str)
                                  (map make-tape
                                       (make-list (- (tape-count) 1) "")))))))

;; Determine if the evaluation of the program is complete, that is, are all the
;; configurations in CONFIGS which do not have any children in a halted state.
;; (engine-done?) -> boolean
(define (engine-done?)
  (define done #t)
  (define (aux tree)
    (when done
      (if (tree-has-children? tree)
          (let loop ((children (tree-children tree)))
            (unless (or (not done)
                        (null? children))
              (aux (car children))
              (loop (cdr children))))
          (unless (halt-state? (config-state (tree-root tree)))
            (set! done #f)))))
  (aux configs)
  done)

;; Get the subtree of CONFIGS whose root is the next configuration to be
;; evaluated by performing a depth first search and taking the first
;; configuration which does not have any children and is not in a halted state.
;; (next-config-tree) -> tree | false
(define (next-config-tree)
  (define target #f)
  (define (aux tree)
    (if (tree-has-children? tree)
        (let loop ((children (tree-children tree)))
          (unless (or target
                      (null? children))
            (aux (car children))
            (loop (cdr children))))
        (unless (halt-state? (config-state (tree-root tree)))
          (set! target tree))))
  (aux configs)
  target)

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

;; Get a list of copies of the rules in RULES with a current state and read
;; symbols equal to those of CONFIG.
;;
;; This procedure replaces any wildcards in the read/write symbols of the copied
;; rules with the read symbols of CONFIG before checking for a match.
;;
;; If the simulator is in deterministic mode, this procedure stops after finding
;; the first matching rule.
;;
;; (find-rules config) -> list
(define (find-rules config)
  (define state (config-state config))
  (define read-symbols (config-read-tapes config))
  (let loop ((rules rules))
    (if (null? rules)
        '()
        (let ((rule (replace-wildcards (car rules) read-symbols)))
          (if (and (state=? state (rule-current-state rule))
                   (equal? read-symbols (rule-read-symbols rule)))
              (cons rule (if (deterministic)
                             '()
                             (loop (cdr rules))))
              (loop (cdr rules)))))))

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

;; Write the tapes, move the heads, and set the state of CONFIG based on RULE.
;; (update-config! config rule) -> void
(define (update-config! config rule)
  (write-tapes! config (rule-write-symbols rule))
  (move-heads! config (rule-move-directions rule))
  (config-state-set! config (rule-next-state rule)))

;; Perform a single step of the evaluation of the program.
;; (engine-step!) -> void
(define (engine-step!)
  (define tree (next-config-tree))
  (define config (tree-root tree))
  (define rules (find-rules config))
  (if (null? rules)
      (config-error! config)
      (if (= (length rules) 1)
          (update-config! config (car rules))
          (let loop ((r (reverse rules)))
            (unless (null? r)
              (let ((c (config-copy config)))
                (update-config! c (car r))
                (tree-cons-child! tree (make-tree c)))
              (loop (cdr r)))))))

;; Perform the entire evaluation of the program.
;; (engine-skip!) -> void
(define (engine-skip!)
  (engine-step!)
  (unless (engine-done?)
    (engine-skip!)))
