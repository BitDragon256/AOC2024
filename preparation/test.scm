(define-structure (btree left v right))

(define (height t)
  (cond
    [(null? t) 0]
    [else (max
      (+ 1 (height (btree-left t)))
      (+ 1 (height (btree-right t)))
  )]))

(define (gen-tree h)
  (cond
  [(= h 0) '()]
  [(make-btree (gen-tree (- h 1)) (random 100) (gen-tree (- h 1)))]))

(define
  (trav-left t f)
  (cond
    [(btree? t) (begin
      (f (btree-v t))
      (trav-left (btree-left t) f)
      (trav-left (btree-right t) f))]
    [else '()]))

(define
  (trav-right t f)
  (when (btree? t)
    (begin
      (f (btree-v t))
      (trav-right (btree-right t) f)
      (trav-right (btree-left t) f))))

(define
  (invert-tree t)
  (if (btree? t)
    (make-btree
      (invert-tree (btree-right t))
      (btree-v t)
      (invert-tree (btree-left t)))
    '()))

(define
  (flatmap-tree t)
  (let ((fmh
    (lambda (l t f)
    (if (btree? t)
      (append
        l
        (list (btree-v t))
        (f '() (btree-left t) f)
        (f '() (btree-right t) f))
      '()))))
    (fmh '() t fmh)))

(define
  (trav-tree t f exec)
  (if
    (btree? t)
    (exec
      (lambda () (trav-tree (btree-left t) f exec))
      (lambda () (f (btree-v t)))
      (lambda () (trav-tree (btree-right t) f exec)))))

(set! a (gen-tree 4))

(define (td v) (begin (display v) (display " ")))

(display "left traverse:\n")
(trav-left a td)
(newline)

(display "general traversal left:\n")
(trav-tree a td (lambda (l v r) (begin (v) (l) (r))))
(newline)

(display "right traverse:\n")
(trav-right a td)
(newline)

(display "general traversal right:\n")
(trav-tree a td (lambda (l v r) (begin (v) (r) (l))))
(newline)

; (display (flatmap-tree a))