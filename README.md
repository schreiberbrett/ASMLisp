# ASMLisp
ASMLisp is an experimental programming language created in order to explore the power of functional programming within assembly.

## Motivation
- LISP and assembly languages have a similar reverse Polish notation syntax.
- LISP symbols and CPU registers are both typeless.
- Functional closures in imperative languages like JavaScript are extremely powerful.
- Straightforward implementation: inline all function calls.


## Examples
There are many assembler macro systems which allow defining macro instructions, for example, incrementing a register. In ASMLisp, this can be achieved with:
```
(define increment (λ register
    (addi register register 1)))
```

ASMLisp takes this principle a step further by allowing code to be passed as data. This leads to more powerful functions like the following:
```
(define doTwice (λ do
    (do)
    (do)))
```

`doTwice` then allows creating an `incrementTwice` function as the following:
```
(define incrementTwice (λ register
    (doTwice (λ
        (increment register)))))
```


### Non-Local Jumps
Non-local jumps can be used for exception handling. Consider the use-case of checking if the index is out of bounds before an array access. In this example, `out-of-bounds-handler-label` is defined in the outer scope and passed in to handle an exception:
```
(define safe-array-access (\ load-into array length index out-of-bounds-handler-label
    (jlt length index out-of-bounds-handler-label)
    (define address load-into)
    (add address array index)
    (lw load-into address)))
```

## Grammar
```
    <program> ::= <definition>*
<instruction> ::= "(" (<definition> | <label> | <call>) ")"
 <definition> ::= "define" <identifier> <argument>
      <label> ::= "label" <identifier>
       <call> ::= <identifier> <argument>*
   <argument> ::= <identifier> | <lambda> | <immediate>
     <lambda> ::= "(" ("lambda" | "λ") <identifier>* <instruction>* ")"
 <identifier> ::= /[a-zA-Z]*/
  <immediate> ::= /[1-9]\d*/
```

## Semantic Rules
- Global scope consistes of all top-level definitions in any order.
- Local scope consists of all the `define` statements executed within the block of instructions (or within its parent's local scope). at up to the instruction in question.
- There must be a `main` lambda in global scope with no arguments.
- An identifier must be in local or global scope before it can be resolved.
