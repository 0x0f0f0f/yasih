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
    (lambda args
        (apply func (cons x args))))

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

; Catamorphisms

; foldr
(define (foldr func end lst) 
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

; foldl 
(define (foldl func accum lst)
    (if (null? lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

; standard naming convention
(define fold foldl)
(define reduce fold)

; sum, product, and, or
(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))


; Anamorphisms
(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

; maximum of a list of arguments
(define (max first . num-list)
    (fold (lambda (old new)
            (if (> old new) old new))
        first
        num-list))

; minimum of a list of arguments
(define (min first . num-list)
    (fold (lambda (old new)
            (if (> old new) old new))
        first
        num-list))

; list length, fold an accumulator over a list counting elements
(define (length lst)
    (fold (lambda (x y)
            (+ x 1))
        0
        lst))

; map a function over a list
(define (map f lst) 
    (foldr (lambda (x y) (cons (func x) y))) '() lst)
    
; filter a list with a predicate. return a list 
; composed only of the elements that satisfy pred
(define (filter pred lst)
    (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

; list append 
(define (append lst . lsts)
    (foldr (flip (curry foldr cons)) lst lsts))


; list reverse
(define (reverse lst)  
    (fold (flip cons) '() lst))

