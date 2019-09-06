(define (reverse lst) (if (null? lst) lst (append (reverse (cdr lst)) (list (car lst)))))
(reverse '(1 2 3 4 5 "a" "b" "c" "d"))