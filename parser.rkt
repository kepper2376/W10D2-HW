#lang racket
(require "util.rkt")

; Parse statements in the professor's language into expression forms.
(define (parse exp)
  (cond
    [(symbol? exp) (list 'var-exp exp)]
    [(number? exp) (list 'num-exp exp)]
    [(string? exp) (list 'string-exp exp)]
    [(null? exp) (error 'parse "empty statement")]
    [(eq? (car exp) 'main)
     (cons 'list-exp (map parse (cdr exp)))]
    [(and (= (length exp) 3) (eq? (cadr exp) '=))
     (list 'assign-exp (list (parse (car exp)) (parse (caddr exp))))]
    [(and (= (length exp) 3) (is_valid_math_op (cadr exp)))
     (list 'math-exp (cadr exp) (parse (car exp)) (parse (caddr exp)))]
    [(and (= (length exp) 3) (is_valid_boolean_op (cadr exp)))
     (list 'boolean-exp (cadr exp) (parse (car exp)) (parse (caddr exp)))]
    [(and (= (length exp) 2) (eq? (car exp) '!))
     (list 'boolean-exp '! (parse (cadr exp)) (void))]
    [(eq? (car exp) 'ask)
     (list 'ask-exp (parse (cadr exp))
           (cons 'list-exp (map parse (caddr exp)))
           (cons 'list-exp (map parse (cadddr exp))))]
    [(eq? (car exp) 'while)
     (list 'while-exp (parse (cadr exp))
           (cons 'list-exp (map parse (caddr exp))))]
    [(eq? (car exp) 'do-while)
     (list 'do-while-exp
           (cons 'list-exp (map parse (cadr exp)))
           (parse (caddr exp)))]
    [(eq? (car exp) 'for)
     (list 'for-exp
           (cons 'list-exp (map parse (cadr exp)))
           (parse (caddr exp))
           (cons 'list-exp (map parse (cadddr exp)))
           (cons 'list-exp (map parse (list-ref exp 4))))]
    [(eq? (car exp) 'fire) (list 'fire-exp (parse (cadr exp)))]
    [(eq? (car exp) 'func)
     (list 'assign-exp
           (list (parse (cadr exp))
                 (list 'new-func-exp
                       (map parse (caddr exp))
                       (cons 'list-exp (map parse (cadddr exp))))))]
    [(eq? (car exp) 'return)
     (list 'return-exp (parse (cadr exp)))]
    [(eq? (car exp) 'call)
     (list 'call-exp (parse (cadr exp)) (map parse (caddr exp)))]
    [else (error 'parse "unsupported statement: ~a" exp)]))

(provide (all-defined-out))
