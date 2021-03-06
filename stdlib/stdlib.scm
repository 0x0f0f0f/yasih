(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

; MACROS
(define (do . forms)
    (last forms))

; while loop
(define-syntax (while c b) ((lambda (cond body) `(begin (define (while-loop) (if ,cond (begin ,body (while-loop)) '())) (while-loop))) c b))

(define (begin . forms)
    (last forms))

(define-syntax (bind-vars bindings)
    `(map car bindings))

(define-syntax (bind-vals bindings)
    `(map cadr bindings))

(define-syntax (let bindings body)
    `(apply (lambda ,(bind-vars bindings) ,body) ',(bind-vals bindings)))

(define (printenv) ; pretty-print the current lexical environment
    (map println (env)))

; 50 ciphers of pi
(define pi 3.14159265358979323846264338327950288419716939937510)

; boolean negation
(define (not x)
    (if x #f #t))

; check if a list is the empty list
(define (null? x)
    (if (eqv? x '()) #t #f))

; list constructor using varargs
(define (list . objs) objs)

; vector constructor using varargs
(define (vector . objs) (list->vector objs))

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
    (foldr (lambda (x y) (cons (f x) y)) '() lst))

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

; list searching 

; helper function, accepts a predicate and an operation for the next element
; the accumulator acc represents the first value found, starts with #f
; and takes the first value that satisfies the predicate
; avoid finding subsequent values by testing for a non false value
; and returning the existing accumulator if set 
(define (mem-helper pred op)
    (lambda (acc next)
        (if (and (eq? acc #f) (pred (op next)))
            next
            acc)))

; Return the first sublist of lst whose car is eq? to x where the sublists of 
; lst are non-empty lists. 
; If x does not occur in lst, then #f (not the empty list) is returned.)
(define (memq x lst)
    (fold (mem-helper (curry eq? x) id) #f lst))

; same as memq but use eqv? for equivalence testing
(define (memv x lst)
    (fold (mem-helper (curry eqv? x) id) #f lst))

; same as memq but use equal? for equivalence testing
(define (member x lst)
    (fold (mem-helper (curry equal? x) id) #f lst))

; assq, assv and assoc take an alist and a key as arguments and return the
; entry for that key if an entry exists, or #f if there is no entry for 
; that key. Note that, in the cases where an entry exists, these procedures
;  return the complete entry, that is (KEY . VALUE), not just the value.
; for example: (assq 2 '((1 . "a") (2 . "b"))) => (2 . "b")

(define (assq x lst)
    (fold (mem-helper (curry eq? x) car) #f lst))

; same as memq but use eqv? for equivalence testing
(define (assv x lst)
    (fold (mem-helper (curry eqv? x) car) #f lst))

; same as memq but use equal? for equivalence testing
(define (assoc x lst)
    (fold (mem-helper (curry equal? x) car) #f lst))

; string constructors
(define (list->string lst) (apply string lst))

; extract char number k from string
(define (string-ref str k) (list-ref (string->list str) k))
