(define (map f l) (if (eq? l '()) l (cons (f (car l)) (map f (cdr l)))))
(map (lambda (x) (+ 1 x)) '(1 2 3 4))
