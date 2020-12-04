# Simple Lisp (`slisp`) Interpreter on Haskell

This repository contains interpreter of simple `lisp-like` language called `slisp`.

    - The parsing is done using `parser combinators`

## Specification

[Formal Syntax of Scheme](https://www.scheme.com/tspl2d/grammar.html)

```xml
<definition>
  = (def <variable> <expression>)
  | (def (<variable>*) <body>)
  |	(def (<variable> <variable>*) <body>)

<variable> = <identifier>

<body> = <definition>* <expression>+

<expression>
  = <constant>
  | <variable>
  | <application>
  | <definition>

<application> = (<expression> <expression>*)
```

## Example code

```lisp
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
```
