(define (qsort l)
    (let ((lesser '()) (greater '()))
        (cond
            ((null? l) '())
            (else (map (lambda (el)
                        (if (> (car l) el)
                            (set! lesser (cons el lesser))
                            (set! greater! (cons el greater))))
                    (cdr l))
                (append (qsort lesser) (cons (car l) (qsort greater)))))))
        
