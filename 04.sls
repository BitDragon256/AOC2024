(load "irregex.scm")
(load "utils.sls")

(set! in (read-file "input/04.txt"))

(define (xmas-count line) (length (irregex-extract "XMAS" line)))
(define (xmas-counts lines) (fold-left (lambda (a l) (+ a (xmas-count l))) 0 lines))

(define (reverse-string str) (list->string (reverse (string->list str))))
(define (string-empty? str) (string=? str ""))
(define (string-car str) (if (string-empty? str) "" (string (car (string->list str)))))
(define (string-cdr str) (list->string (cdr (string->list str))))
(define (sum l) (fold-left + 0 l))
(define (all l) (fold-left (lambda (a b) (and a b)) #t l))
(define (any l) (fold-left (lambda (a b) (or a b)) #f l))

(define (rotate90 lines)
    (define (h l)
        (if
            (any (map string-empty? l))
            (list l)
            (append
                (list l)
                (h (map string-cdr l)))))
    (map (lambda (line) (strlist->string (map string-car line)))
    (filter (lambda (line) (all (map (lambda (p) (not (string-empty? p))) line))) (h lines))))

(define (cut1 l)
    (filter (lambda (el) (not (string=? el ""))) (map string-cdr l)))

(define (rotate45 lines)
    (define (rotate45 lrem acc)
        ; (begin
        ;     (display "lrem: ")
        ;     (display lrem)
        ;     (display ", acc: ")
        ;     (print acc)
        (cond
            [(and (empty? lrem) (empty? (car acc))) acc]
            [(empty? lrem) (rotate45 '() (cons (cut1 (car acc)) acc))]
            [(empty? acc) (rotate45 (cdr lrem) (list (list (car lrem))))]
            [else (rotate45 (cdr lrem) (cons (append (list (car lrem)) (cut1 (car acc))) acc))]))
        ; )
    (map strlist->string (reverse (map (lambda (line) (map string-car line)) (cdr (rotate45 lines '()))))))

(define (total-xmas-count lines)
    (+
        (xmas-counts lines)
        (xmas-counts (map reverse-string lines))
        (xmas-counts (rotate90 lines))
        (xmas-counts (map reverse-string (rotate90 lines)))
        (xmas-counts (rotate45 lines))
        (xmas-counts (map reverse-string (rotate45 lines)))
        (xmas-counts (rotate45 (map reverse-string (rotate90 lines))))
        (xmas-counts (map reverse-string (rotate45 (map reverse-string (rotate90 lines)))))
    ))

(print (total-xmas-count in))

#|
(display "lines: ")
(print in)
(display "rev: ")
(print (map reverse-string in))
(display "90: ")
(print (rotate90 in))
(display "rev 90: ")
(print (map reverse-string (rotate90 in)))
(display "45: ")
(print (rotate45 in))
(display "rev 45: ")
(print (map reverse-string (rotate45 in)))
(display "45 90: ")
(print (rotate45 (map reverse-string (rotate90 in))))
(display "rev 45 90: ")
(print (map reverse-string (rotate45 (map reverse-string (rotate90 in)))))
|#