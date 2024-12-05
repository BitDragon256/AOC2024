(load "utils.sls")
(load "irregex.scm")

(display "1: ")
(print
  (fold-left
  (lambda (acc mul)
    (+ acc
    (let [(s (string-split mul #\,))]
      (*
        (string->number (car (irregex-extract "[0-9]+" (car s))))
        (string->number (car (irregex-extract "[0-9]+" (cadr s)))))
    )))
  0
  (irregex-extract "mul\\([0-9]+,[0-9]+\\)" (car (read-file "input/03.txt")))))
  
(display "2: ")
(print
  (fold-left
  (lambda (acc mul)
    (+ acc
    (let [(s (string-split mul #\,))]
      (*
        (string->number (car (irregex-extract "[0-9]+" (car s))))
        (string->number (car (irregex-extract "[0-9]+" (cadr s)))))
    )))
  0
  (irregex-extract "mul\\([0-9]+,[0-9]+\\)" (fold-left string-append "" (irregex-extract "do\\(\\)(?:(?!don't\\(\\)).)*" (car (read-file "input/03.txt")))))))

; (print (irregex-extract "mul\\([0-9]+,[0-9]+\\)" (fold-left string-append "" (irregex-extract "do\\(\\)(?:(?!don't\\(\\)).)*" (cadr (read-file "input/ex03.txt"))))))