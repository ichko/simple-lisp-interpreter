# Simple Lisp (SLISP) Interpreter in Haskell

This repository contains interpreter of simple `lisp-like` language called `slisp`.

    - The parsing is done using `parser combinators`

## Specification

[Formal Syntax of Scheme](https://www.scheme.com/tspl2d/grammar.html)

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

## Example code

```lisp
(define two 2)

(define (twice n) (* n two))

(define a -10)

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

```js
[Variable "two" (Constant (IntValue 2)),Function "" ["twice","n"] [Application "*" [Reference "n",Reference "two"]],Variable "\1072" (Constant (IntValue (-10))),Function "" ["fib","n"] [Application "if" [Application "<=" [Reference "n",Constant (IntValue 2)],Constant (IntValue 1),Application "+" [Application "fib" [Application "-" [Reference "n",Constant (IntValue 1)]],Application "fib" [Application "-" [Reference "n",Constant (IntValue 2)]]]]],Function "" ["factorial","n"] [Application "if" [Application "==" [Reference "n",Constant (IntValue 0)],Constant (IntValue 1),Application "*" [Reference "n",Application "factorial" [Application "-" [Reference "n",Constant (IntValue 1)]]]]],Application "define" [Application "main" [],Variable "fact" (Application "factorial" [Application "fib" [Constant (IntValue 10)]]),Application "print" [Reference "a"],Application "print" [Application "fib" [Constant (IntValue 10)]],Application "print" [Application "twice" [Constant (IntValue 5)]],Application "print" [Reference "fact"]]]
```
