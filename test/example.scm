(define two 2)

(define twice (n) (* n two))

(define a -10)

(define fib (n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(define factorial (n)
  (if (<= n 0)
    1
    (* n (factorial (- n 1)))
  )
)

(define main ()
  ; (define ten (twice 5))
  ; (print [(fib 5), (fact 6), ten])
  (fib 5)
)

