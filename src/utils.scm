;;;; utils.scm - Various utility procedures.

(declare (unit utils))

;;; list -----------------------------------------------------------------------

;; Get a new list containing the first N elements of LST. N must be inside the
;; range [0, L] where L is the length of LST.
;; (list-head list integer) -> list
(define (list-head lst n)
  (if (zero? n)
      '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

;; Get a new list containing the unique elements of LST.
;; (list-unique list) -> list
(define (list-unique lst)
  (define elems '())
  (define (aux lst)
    (unless (null? lst)
      (unless (member (car lst) elems)
        (set! elems (cons (car lst) elems)))
      (aux (cdr lst))))
  (aux lst)
  (reverse elems))

;;; string ---------------------------------------------------------------------

;; Get the first character of STR, or false if STR is empty.
;; (first-character string) -> character | false
(define (first-character str)
  (if (= (string-length str) 0)
      #f
      (string-ref str 0)))

;; Get the first non-whitespace character of STR, or false if STR does not
;; contain any non-whitespace characters.
;; (first-non-whitespace string) -> character | false
(define (first-non-whitespace str)
  (let ((len (string-length str))
        (char #f))
    (let loop ((i 0))
      (unless (or char
                  (= i len))
        (let ((c (string-ref str i)))
          (unless (char-whitespace? c)
            (set! char c)))
        (loop (+ i 1))))
    char))

;;; tree -----------------------------------------------------------------------

;; Trees are represented by lists, where the first element of the list is the
;; root of the tree, and subsequent elements are its children.

;; Make a tree containing ELEMS.
;; (make-tree any*) -> tree
(define (make-tree . elems)
  elems)

;; Determine if ELEM is a tree.
;; (tree? any) -> boolean
(define (tree? elem)
  (list? elem))

;; Determine if TREE has children.
;; (tree-has-children? tree) -> boolean
(define (tree-has-children? tree)
  (not (null? (tree-children tree))))

;; Get the root of TREE.
;; (tree-root tree) -> any
(define (tree-root tree)
  (car tree))

;; Get the children of TREE.
;; (tree-children tree) -> list
(define (tree-children tree)
  (cdr tree))

;; Cons CHILD onto the children of TREE.
;; (tree-cons-child! tree) -> void
(define (tree-cons-child! tree child)
  (set-cdr! tree (cons child (tree-children tree))))

;; Perform a preorder traversal of TREE and return a list containing the visited
;; elements in the order they were visited.
;; (tree-preorder tree) -> list
(define (tree-preorder tree)
  (if (tree? tree)
      (cons (tree-root tree)
            (apply append (map tree-preorder (tree-children tree))))
      (list tree)))

;; Get a list containing the leaf nodes of TREE.
;; (tree-leaves tree) -> list
(define (tree-leaves tree)
  (define lst '())
  (define (aux tree)
    (if (tree-has-children? tree)
        (for-each aux (tree-children tree))
        (set! lst (cons tree lst))))
  (aux tree)
  (reverse lst))
