(define (two) 2)

(define (five) (+ 3 (two)))

(define (++ a) (+ 1 a))

(define (main) (++ (five)))
