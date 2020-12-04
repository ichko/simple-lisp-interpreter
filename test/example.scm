(define two 2)

(define (twice n) (* n two))

(define а -10)

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(define (factorial n)
  (if (== n 0)
    1
    (* n (factorial (- n 1)))
  )
)

(define (main)
  (define fact (factorial (fib 10)))
  (print a)
  (print (fib 10))
  (print (twice 5))
  (print fact)
)
