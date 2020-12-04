# Simple Lisp (SLISP) Interpreter in Haskell

This repository contains interpreter of simple `lisp-like` language called `slisp`.

- The parsing is done using `parser combinators`

## Running the parser

```hs
runParser program "(def a (sum 1 ab (+ 1 1)))"
> Variable "a" (Application "sum" [Constant (IntValue 1),Reference "ab",Application "+" [Constant (IntValue 1),Constant (IntValue 1)]])]
```

Run the tests to see example of parsed `SLISP`

```bash
stack test
```

## Specification

The specification of SLISP simpler version of the specification of `scheme` defined here - [Formal Syntax of Scheme](https://www.scheme.com/tspl2d/grammar.html).

```xml
<program> = <expression>*

<expression>
  = <constant>
  | <reference>
  | <application>
  | <variable-def>
  | <function-def>

<constant> = <integer> | <boolean>

<reference> = <identifier>

<identifier> = <char-identifier>*

<char-identifier>
  = a | b | .. | z
  | 1 | 2 | .. | 9
  | + | - | * | / | > | < | =

<application> = (<identifier> <expression>*)

<variable-def>
  = (define <identifier> <expression>)

<function-def>
  = (define <identifier> (<identifier>*) <expression>+)
```

Data types defined in Haskell

```hs
type Program = [Expression]

type Identifier = String

data Expression
  = Constant Value
  | Reference Identifier
  | Application Identifier [Expression]
  | Variable Identifier Expression
  | Function Identifier [Identifier] [Expression]
  deriving (Show)

data Value
  = IntValue Integer
  | BoolValue Bool
  deriving (Show)
```

## Some SLISP Code

```scheme
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
  (if (== n 0)
    1
    (* n (factorial (- n 1)))
  )
)

(define main ()
  (define fact (factorial (fib 10)))
  (print a)
  (print (fib 10))
  (print (twice 5))
  (print fact)
)
```
