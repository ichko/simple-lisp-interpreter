(define (two) 2)

(define (twice n) (* n (two)))

(define (a) -10)

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(define (main)
  (define (ten) (twice 5))
  (fib (ten))
)

