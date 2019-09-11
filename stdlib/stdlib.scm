; boolean negation
(define (not x) 
    (if x #f #t))

; check if a list is the empty list
(define (null? x) 
    (if (eqv? x '()) #t #f))

; list constructor using varargs
(define (list . objs) objs) 

; identity function 
(define (id obj) obj)

; flip a function argument order
(define (flip func)
    (lambda (x y)
        (func y x)))

; function curry-ing (partial application)
(define (curry func x)
    (lambda (arg)
        (apply func (cons x arg))))

; function composition
(define (compose f g)
    (lambda (arg)
        (f (apply g arg))))

; Simple numerical functions
(define zero? (curry = 0)) ; a number is zero
(define positive? (curry < 0)) ; a number is positive
(define negative? (curry > 0))
(define (odd? num) (= (modulo num 2) 1))
(define (even? num) (= (modulo num 2) 0))

; map a function over a list
(define (map f l) 
    (if (null? l)
        l 
        (cons (f (car l)) (map f (cdr l)))))

; list reverse
(define (reverse lst)  
    (if (null? lst) 
        lst 
        (append (reverse (cdr lst)) (list (car lst)))))
