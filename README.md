# ASMLisp
ASMLisp is an experimental programming language created in order to explore the power of functional programming within assembly.

## Motivation
- LISP and assembly languages have a similar reverse Polish notation syntax.
- LISP symbols and CPU registers are both typeless.
- Functional closures in imperative languages like JavaScript are extremely powerful.
- Straightforward implementation: inline all function calls.


## Examples
There are many assembler macro systems which allow defining macro instructions, for example, incrementing a register. In ASMLisp, this can be achieved with:
```lisp
(define increment (λ register
    (addi register register 1)))
```

ASMLisp takes this principle a step further by allowing code to be passed as data. This leads to more powerful functions like the following:
```lisp
(define doTwice (λ do
    (do)
    (do)))
```

`doTwice` then allows creating an `incrementTwice` function as the following:
```lisp
(define incrementTwice (λ register
    (doTwice (λ
        (increment register)))))
```
