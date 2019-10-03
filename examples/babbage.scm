;; Charles Babbage, looking ahead to the sorts of problems his 
;; Analytical Engine would be able to solve, gave this example:

;; What is the smallest positive integer whose square ends in the digits 269,696?

(define (digits n)
  (string->list (number->string n)))
 
(define (ends-with list tail)
  ;; does list end with tail?
  (starts-with (reverse list)
               (reverse tail)))
 
(define (starts-with list head)
  (cond ((null? head)
         #t)
        ((null? list)
         #f)
        ((equal? (car list) (car head))
         (starts-with (cdr list) (cdr head)))
        (else
         #f)))
 
(define (babbage start) 
  (let loop ((i 1))
    (if (ends-with (digits (* i i)) (digits 269696))
        i
        (loop (+ i 1)))))
