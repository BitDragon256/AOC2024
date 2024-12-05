(load "irregex.scm")
(load "utils.sls")

(set! RULES (read-file "input/05.txt"))
(set! DATA (read-file "input/05_.txt"))

(define (reg-not str) (string-append "(?:(?!" str ").)*"))

(define (rule->regex rule)
  (let [(n (string-split rule #\|))]
    (string-append "(#" (reg-not (cadr n)) (car n) (reg-not (cadr n)) (cadr n) (reg-not (cadr n)) "#)|(#" (reg-not (car n)) "#)|(#" (reg-not (cadr n)) (car n) (reg-not (cadr n)) "#)")))

(define (match? reg str)
  (> (length (irregex-extract reg (string-append "#" str "#"))) 0))

(define (ordered? line rules)
  (all (map (lambda (rule)
    (match? rule line)
    ) rules)))

(display "1: ")
(print
  (sum
    (map (lambda (l) (let [(a (string-split l #\,))] (string->number (list-ref a (floor (/ (length a) 2))))))
      (filter (lambda (l) (ordered? l (map rule->regex RULES))) DATA))))

(define (swap l a b)
  (cond
    [(empty? l) '()]
    [(string=? (car l) a) (cons b (swap (cdr l) a b))]
    [(string=? (car l) b) (cons a (swap (cdr l) a b))]
    [else (cons (car l) (swap (cdr l) a b))]))
    
(define (zip-between l el)
  (if (empty? (cdr l)) (list (car l)) (append (list (car l)) (list el) (zip-between (cdr l) el))))

(define (strlist->string l) (fold-left string-append "" l))

(define (order line rules)
  (define (order corline accrules)
  (cond
    [(empty? accrules) corline]
    [(match? (rule->regex (car accrules)) corline) (order corline (cdr accrules))]
    [else (let
      [(rs (string-split (car accrules) #\|))]
      (order (strlist->string (zip-between (swap (string-split corline #\,) (car rs) (cadr rs)) ",")) rules)
    )]))
  (order line rules))

(print "2: ")
(print
  (sum
    (map (lambda (l) (let [(a (string-split l #\,))] (string->number (list-ref a (floor (/ (length a) 2))))))
    (map (lambda (l) (begin (print (string-append "ordering " l)) (order l RULES)))
    (filter (lambda (l) (not (ordered? l (map rule->regex RULES)))) DATA)))))