(load "utils.sls")

(set! IN (read-file "input/08.txt"))

; small 2d vector library
(define-structure (vec x y))
(define (vec+ a b) (make-vec (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))
(define (vec- a b) (make-vec (- (vec-x a) (vec-x b)) (- (vec-y a) (vec-y b))))
(define (vec* l v) (make-vec (* l (vec-x v)) (* l (vec-y v))))
(define (vec/ v l) (make-vec (/ (vec-x v) l) (/ (vec-y v) l)))
(define (vec= a b) (and (= (vec-x a) (vec-x b)) (= (vec-y a) (vec-y b))))

(define (unique-vecs lst)
  (if (empty? lst) '() (append (list (car lst)) (unique-vecs (filter (lambda (l) (not (vec= (car lst) l))) (cdr lst))))))

; utils
(define (flatten l)
  (cond
    [(not (list? l)) (list l)]
    [(empty? l) '()]
    [else (append (flatten (car l)) (flatten (cdr l)))]))

(define-structure (ant f v))

(define (string->antennas lines)
  (define (string->antennas rem y)
    (define (line->antennas x line)
      (cond
        [(string-empty? line) '()]
        [else
          (append
            (if (string=? (string-car line) ".") '() (list (make-ant (string-car line) (make-vec x y))))
            (line->antennas (add1 x) (string-cdr line)))]))
    (if
      (empty? rem) '()
      (append (line->antennas 0 (car rem)) (string->antennas (cdr rem) (add1 y)))))
  (string->antennas lines 0))

(define (get-antinodes a b)
  (let [(delta (vec- (ant-v a) (ant-v b)))]
  (if (string=? (ant-f a) (ant-f b))
    (list
      (vec+ (ant-v a) delta)
      (vec- (ant-v b) delta))
    '())))

(define (find-all-antinodes antennas)
  (flatten
  (cond
    [(<= (length antennas) 1) '()]
    [else
      (append
        (map (lambda (a) (get-antinodes (car antennas) a)) (cdr antennas))
        (find-all-antinodes (cdr antennas)))])))

(print (length
  (filter (lambda (v) (and (>= (vec-x v) 0) (>= (vec-y v) 0) (< (vec-x v) (length IN)) (< (vec-y v) (length IN))))
  (unique-vecs (find-all-antinodes (string->antennas IN))))))