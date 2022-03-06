# CE (Control and Environment) interpreter for Scheme

This is a CE interpreter for a substantial subset of Scheme/Racket. A CE interpreter is meta-circular to a large degree. For example, a conditional in the target language (scheme-ir?) can be implemented using a conditional in the host language (Racket),recursive evaluation of a subexpression can be implemented as a recursive call to the interpreter, however, it's characterized by creating an explicit closure value for lambdas that saves its static environment (the environment when it's defined). 

## Input language

Following is a structure for the target language the interpreter supports. It supports any syntax allowed by scheme-ir that runs without error in Racket, returning a correct value.

Also supports the following primitive functions bound to following variables at the top-level:

```
+ - * = equal? list cons car cdr null?)
```

```
e ::= (lambda (x) e))
		| (lambda (x ...) e)
    | (let ([x e] ...) e)
    | (let* ([x e] ...) e)
    | (apply e e)
    | (e e ...)
    | x
    | (and e ...) 
    | (or e ...)
    | (if e e e)
    | (prim e) | (prim e e)
    | datum

datum ::= nat | (quote ()) | #t | #f
nat ::= 0 | 1 | 2 | ...
x is a symbol
prim is a primitive function.
```

The interpreter supports syntax from target language such as let (with any number of bindings, including zero), let*, lambda, application, variables, and/or (short-circuiting as necessary), if, applications of unary and binary, primitive functions, and constants (called "datums").

