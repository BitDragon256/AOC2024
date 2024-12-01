(load "utils.sls")

(define example (list "3 4" "4 3" "2 5" "1 3" "3 9" "3 3"))

(define
  (load-ids lines)
  (fold-left
    (lambda (acc l)
      (cons
        (cons (string->number (car l)) (car acc))
        (cons (string->number (cadr l)) (cdr acc)))
      )
    (cons '() '())
    (map
      (lambda (l) (string-split l #\ ))
      lines)))

(set! ids (load-ids (read-file "input/01.txt")))
; (set! ids (load-ids example))
(set! ll (sort < (car ids)))
(set! rl (sort < (cdr ids)))

(define
  (distance l r)
  (fold-left + 0
    (map (lambda (a b) (abs (- a b))) l r)))
  
(print (string-append "part 1: " (number->string (distance ll rl))))

; (listof Number) -> (listof (pair Number Number))
; counts the occurences of each element in a sorted list
(define (counts l)
  (define (counts cur acc rem)
    (if
      (empty? rem) acc
      (if
        (= cur (car rem)) (counts cur (cons (cons cur (add1 (cdar acc))) (cdr acc)) (cdr rem))
        (counts (car rem) (cons (cons (car rem) 1) acc) (cdr rem)))))
  (counts 0 '() l))

; Number (listof (pair Number Number)) -> Number
; finds the count of occurences of a number with the given list of counts (see counts)
(define (find-count v l)
  (cond
    [(empty? l) 0]
    [(= v (caar l)) (cdar l)]
    [else (find-count v (cdr l))]))

(define (similarity-score l r)
  (let [(cr (counts r))]
    (fold-left + 0
      (map
        (lambda (v)
          (* v (find-count v cr)))
        l))))

(print (string-append "part 2: " (number->string(similarity-score ll rl))))