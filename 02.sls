(load "utils.sls")

(define
  (load-reports lines)
  (map
    (lambda (l) (map (lambda (lev) (string->number lev)) l))
  (map
    (lambda (l) (string-split l #\ ))
    lines)))

(define (safe? levels pred)
  (define (safe? old newl)
    (if (empty? newl) #t
      (let [(new (car newl))]
        (if (pred old new) (safe? new (cdr newl)) #f))))
  (safe? (car levels) (cdr levels)))

(define (dec-safe a b) (and (> a b) (<= a (+ b 3))))
(define (inc-safe a b) (dec-safe b a))

(define (safe-report? reports)
  (map
    (lambda
      (levels)
      (or (safe? levels dec-safe) (safe? levels inc-safe)))
    reports))

(display "1: ")
(print (fold-left + 0 (map (lambda (a) (if a 1 0)) (safe-report? (load-reports (read-file "input/02.txt"))))))

(define (safed? levels pred)
  (define (safed? old newl s)
    (if (empty? newl) #t
      (let [(new (car newl))]
        (cond
          [(pred old new) (safed? new (cdr newl) s)]
          [s #f]
          [else (safed? old (cdr newl) #t)]))))
  (safed? (car levels) (cdr levels) #f))

(define (all l) (fold-left (lambda (a b) (and a b)) #t l))
(define (any l) (fold-left (lambda (a b) (or a b)) #f l))

(define (rem-fe l)
  (define (rem-fe f v t acc)
    (append acc
      (cond
        [(empty? t) (list f)]
        [else (append (list (append f t)) (rem-fe (append f (list v)) (car t) (cdr t) acc))])))
  (rem-fe '() (car l) (cdr l) '()))

(define (safed-report? reports)
  (map
    (lambda
      (levels)
      (or (safed? levels dec-safe) (safed? levels inc-safe)))
    reports))

(define (safed-report2? reports)
  (map
    (lambda
      (levels)
      ; (begin
      ; (display (rem-fe levels))
      ; (display ": ")
      ; (print (any (map (lambda (v) (or (safe? v dec-safe) (safe? v inc-safe))) (rem-fe levels))))
      (any (map (lambda (v) (or (safe? v dec-safe) (safe? v inc-safe))) (rem-fe levels))))
      ; )
    reports))

(display "2: ")
(print (fold-left + 0 (map (lambda (a) (if a 1 0)) (safed-report2? (load-reports (read-file "input/02.txt"))))))