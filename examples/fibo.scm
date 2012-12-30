(define (next str)
  (progn
    (define w (car (cdr str)))
    (w (car str))))

(define (first str)
  (car str))
  
(define (gen x) (list x (lambda (k) (gen (+ x 1)))))

(define (fibo) 
  (progn
   (define (fibo-next x y) (list (+ x y) (lambda (k) (fibo-next y (+ x y)))))
   (list 1 (lambda (k) (list 1 (lambda (k) (fibo-next 1 1)))))
  ))
(define (square str) (list (* (first str) (first str)) (lambda (k) (square (next str)))))
(define (take n str)
  (if (= n 0)
      () 
      (list (first str) (lambda (x) (take (- n 1) (next str))) )
      ))

(define (print-str str)
  (if (null? str)
      ()
      (progn
       (write (first str))
       (newline)    
       (print-str (next str)))))
           


(define s (square (gen 5)))

(print-str (take 10 s))
(newline)
(print-str (take 20 (fibo)))
(newline)
(write (first s))
(newline)
(write (first (next s)))


