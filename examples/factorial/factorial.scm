(define fact1 
        (lambda (x)
            (if (> x 0)
                (* x (fact1 (- x 1)))
                1))) 

(write (fact1 5))
(newline)
(define (fact2 x) 
        (if (> x 0)
            (* x (fact2 (- x 1)))
            1)) 

(write (fact2 5))
