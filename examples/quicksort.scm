(define (partition pred xs)
   (let part ((ps '()) (ns '()) ; Initial "positives" `ps`, and "negatives" `ns`
              (xs' xs))
      (if (null? xs')
         (cons ps ns)             ; Returning pair of lists
         (let ((x (car xs')))     ; Memoization of `(car lst)`
            (if (pred x)
               (part (cons x ps) ns (cdr xs'))
               (part ps (cons x ns) (cdr xs')))))))

(define (quicksort xs)
   (if (null? xs) '()
      (let* ((x (car xs))
             (pn (partition               ; Memoization of `partition`
                    (lambda (x')
                       (< x' x) )
                    (cdr xs) )))
         (append (quicksort (car pn))      ; Extracting positives from pair
                 (list x)                  ; Pivot
                 (quicksort (cdr pn)) )))) ; Negatives


(quicksort (list 4 2 3 5 1))
