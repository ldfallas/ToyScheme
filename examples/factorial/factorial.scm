(define fact1 
        (lambda (x)
            (if (> x 0)
                (* x (fact1 (- x 1)))
                1))) 

(write (fact1 5))
