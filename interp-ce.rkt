#lang racket

;; A CE (Control and Environment) interpreter for Scheme

(provide interp-ce)

; A CE interpreter is meta-circular to a large degree (e.g., a conditional in the target
; language (scheme-ir?) can be implemented using a conditional in the host language (Racket),
; recursive evaluation of a sub-expression can be implemented as a recursive call to the
; interpreter, however, it's characterized by creating an explicit closure value for lambdas
; that saves its static environment (the environment when it's defined). For example, a CE
; interpreter for the lambda calculus may be defined 
(define (interp-ce-lambda exp [env (hash)])
  (match exp
         [`(lambda (,x) ,body)
          ; Return a closure that pairs the code and current (definition) environment
          `(closure (lambda (,x) ,body) ,env)]
         [`(,efun ,earg)
          ; Evaluate both sub-expressions
          (define vfun (interp-ce-lambda efun env))  
          (define varg (interp-ce-lambda earg env))
          ; the applied function must be a closure
          (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
          ; we extend the *closure's environment* and interp the body
          (interp-ce-lambda body (hash-set env+ x varg))]
         [(? symbol? x)
          ; Look up a variable in the current environment
          (hash-ref env x)]))

(define (scheme-ir? exp)
  ; You should support a few built-in functions bound to the following variables at the top-level
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (match exp
         [`(lambda ,(? (listof symbol?)) ,(? scheme-ir?)) #t] ; fixed arguments lambda
         [`(lambda ,(? symbol?) ,(? scheme-ir?)) #t] ; variable argument lambda
         [`(if ,(? scheme-ir?) ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [`(let ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(let* ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(and ,(? scheme-ir?) ...) #t]
         [`(or ,(? scheme-ir?) ...) #t]
         [`(apply ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [(? (listof scheme-ir?)) #t]
         [(? prim?) #t]
         [(? symbol?) #t]
         [(? number?) #t]
         [(? boolean?) #t]
         [''() #t]
         [_ #f]))
(define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points. 
(define (interp-ce exp)
  ; Might add helpers or other code here...
  ;; exp is a Scheme expression that has the form scheme-ir?
  ;; env is a hash from variables to values
  (define (interp exp env)
    (match exp
      ;; how do we handle a variable
      [(? symbol? x) (hash-ref env x)]
      [(? boolean? x) x]
      [''() '()]
      [(? number? n) n]
      [`(lambda (,xs ...) ,body) `(closure ,exp ,env)] 
      ; Untagged application case goes after all other forms
      [`(apply ,ef ,e-arg-list)
       ;; Assume that ef evaluates to a closure
       (define clo-ef (interp ef env))
       ;; argument list
       (define v-args (map (lambda (e-arg) (interp e-arg env))
                           (interp e-arg-list env)))
       (match clo-ef
         [`(closure (lambda (,xs ...) ,e-body) ,clo-env)
          ;; set each of xs to its corresponding position in v-args
          (define new-env
            (foldl (lambda (x v env) (hash-set env x v)) clo-env xs v-args))
          (interp e-body new-env)]
         [_ `(error (format "Expected closure but got ~a" clo-ef))])]
      [`(,ef ,e-args ...)
       ;; Assume that ef evaluates to a closure
       (define clo-ef (interp ef env))
       ;; argument list
       ;;what v-args is really doing is like (map (lambda (e-arg) (interp e-arg (hash))) (rest '((lambda (x y) x) 2 3))) 
       (define v-args (map (lambda (e-arg) (interp e-arg env))
                           e-args))
       (match clo-ef
         [`(closure (lambda (,xs ...) ,e-body) ,clo-env)
          ;; set each of xs to its corresponding position in v-args
          ;;what new-env is really doing is (foldl (lambda (x v env) (hash-set env x v)) #hash() '(x y) '(2 3)) for example
          (define new-env
            (foldl (lambda (x v env) (hash-set env x v)) clo-env xs v-args))
          ;; we can just keep on interp the symbol 'x in the body of lambda
          ;;with new env hash env '#hash((x . 2) (y . 3)), we can use hash-ref
          ;;to get the value bound to the symbol 'x, which is just 2. 
          (interp e-body new-env)]
         ;; if the ef is a procedure to be applied to a list of two elements.
         [(? procedure? f) (f v-args)]
         [_ `(error (format "Expected closure but got ~a" clo-ef))])]))
  ;;handling primitives in the starting env '(+ - * = equal? list cons car cdr null?)
  (interp exp (hash '+ (lambda lst (foldl (lambda (x acc) (+ x acc)) 0 lst))
                    '- (lambda lst (foldl (lambda (x acc) (- acc x)) (car lst) (cdr lst)))
                    'list (lambda (lst) lst)
                    'cons (lambda (lst) (cons (car lst) (cadr lst)))
                    'car (lambda lst (caar lst))
                    'cdr (lambda lst (cdar lst)))))

(interp-ce '((lambda (x y) x) 2 3))
(interp-ce '(apply (lambda (x) x) (cons 1 '())))



