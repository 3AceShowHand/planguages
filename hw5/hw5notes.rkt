#lang racket

(provide (all-defined-out))

; Datatype-Programming in Racket Without Structs

;datatype exp = Const of int | Negate of exp
;             | Add of exp * exp | Multiply of exp * exp

; just helper functions that make lists where first element is a symbol
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

; helper functions that test what "kind of exp"
; Note: More robust could raise better errors for non-exp values
; 'foo is a Racket symbols, compare two symbols with eq?
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

; helper functions that get the pieces for "one kind of exp"
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

; New feature
; (struct foo (bar baz quux) #:transparent)
; (foo e1 e2 e3) returns "a foo" with bar, baz, quux fields holding results of evaluating e1, e2 and e3
; (foo? e) evaluates e and returns #t if and only if the result is something that was made with the foo function
; (foo-bar e) evaluates e. If result was made with the foo function, return the contents of the bar field, else an error

; #:transparent is an optional attribute on struct definitions
; #: mutable is another optional attribute on struct definitions
; (struct card (suit rank) #:transparent #:mutable)
; also defines set-card-suit!, set-card-rank!

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

; Advantages of Structs
; Neither functions nor macros can create a new kind of data
; Result of constructor function returns #f for every other tester functions: number?, pair?, etc.

; Implementing Programming Languages

; Typical workflow
; 1. concrete syntax (string)
; Parsing -> abstract syntax tree -> Type checking? -> Rest of implementation

; "Rest of implementation" takes the AST and then produce a result
; Interpreter or compiler
; Interpreter: Write in "language A", take a programming in B and produce an answer in B.
; Compiler: Write in "language A" to a third "language C"
; Call A the "metalanguage"

; What the interpreter can and cannot assume
; Interpreter can assume input is a "legal AST for B"
;   Okay to give wrong answer or inscrutable error otherwise
; Interpreter must check that recursive results are the right kind of value
;   Give a good error message otherwise

; Legal ASTs
; "Trees the interpreter must handle" are a subset of all the trees
; Can assume "right types" for struct fields
; Illegal ASTs can "crash the interpreter"

; Interpreter results
; Return expressions, but not any expressions
; Result should always be a value, a kind of expression that evaluate to itself
; if not, the interpreter has a bug.

; Implementing Variables and Environments
; An environment is a mapping from variables(Racket strings) to values(as defined by the language)
; Evaluation takes place in an environment
;   Env passed as arugment to interpreter helper function
;   A variable expression looks up the variable in the env
;   Most subexpressions use same env as outer expression
;   A let-expression evaluates its body in a larger env

; Implementing Clousures
; Higher-order functions
; use a closure data structure to keep the environment it will need to use later
; (struct closure (env fun) #:transparent)
; Evaluate a function expression:
; - A function is not a value, a closure is a value
; - Create a closure out of (a) the function and (b) the current environment when the function was evaluated

; We need to create a closure so that when the result is later used in some call somewhere else in
; the program, we have the correct environment. As always, we do not evaluate the function body until the function is called.
; We need the entire function, not just the function body, because we need the name of the function argument.

; Function calls
; (call e1 e2)
; - Use current env to evaluate e1 to a closure, error if result is a value that is not a closure
; - Use current env to evaluate e2 to a value
; - Evaluate closure's function's body in the closure's env.
;   - Map the function's argument-name to the argument-value
;   - And for recursion, map the function's name to the whole closure

; Given a closure, the code part is only ever evaluated using the env part, not the env at the call-site
; Alternative used in practice: When creating a closure, store a possibly-smaller env holding only the variables
; that are free variables in the function body
; - Free variables: Variables that occur, not counting shadowed uses of the same variable name
; - A function body would never need anything else from the environment

; Computing free variables
; Before evaluation begins, compute free variables of every function in program and
; store this information with the function.

; Racket Functions As "Macros" For interpreted language
; A macro should produce an expression in the language being implemented, not evaluate
; such an expression. The purpose of eval-exp is to evaluate expressions.

