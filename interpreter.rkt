#lang racket
(require "util.rkt")

; Every function has a #return slot in its own scope.
(define (has_returned)
  (let ([ret (resolve_env environment '|#return|)])
    (not (or (void? ret) (eq? ret '|#none|)))))

(define (process parsed-exp)
  (cond
    [(void? parsed-exp) (void)]
    [(null? parsed-exp) (void)]
    [(eq? (car parsed-exp) 'var-exp)
     (resolve_env environment (cadr parsed-exp))]
    [(eq? (car parsed-exp) 'num-exp) (cadr parsed-exp)]
    [(eq? (car parsed-exp) 'string-exp) (cadr parsed-exp)]
    [(eq? (car parsed-exp) 'assign-exp)
     (let* ([parts (cadr parsed-exp)]
            [value (process (cadr parts))])
       (wolf_env environment (cadr (car parts)) value))]
    [(eq? (car parsed-exp) 'math-exp)
     (do_math (cadr parsed-exp)
              (process (caddr parsed-exp))
              (process (cadddr parsed-exp)))]
    [(eq? (car parsed-exp) 'boolean-exp)
     (if (eq? (cadr parsed-exp) '!)
         (not (process (caddr parsed-exp)))
         (calculate_boolean (cadr parsed-exp)
                            (process (caddr parsed-exp))
                            (process (cadddr parsed-exp))))]
    [(eq? (car parsed-exp) 'ask-exp)
     (if (process (cadr parsed-exp))
         (process (caddr parsed-exp))
         (process (cadddr parsed-exp)))]
    [(eq? (car parsed-exp) 'list-exp)
     (let loop ([statements (cdr parsed-exp)])
       (unless (or (null? statements) (has_returned))
         (process (car statements))
         (loop (cdr statements))))]
    [(eq? (car parsed-exp) 'while-exp)
     (let loop ()
       (when (and (not (has_returned)) (process (cadr parsed-exp)))
         (push_scope_to_environment '())
         (process (caddr parsed-exp))
         (pop_scope_from_environment)
         (loop)))]
    [(eq? (car parsed-exp) 'do-while-exp)
     (let loop ()
       (unless (has_returned)
         (push_scope_to_environment '())
         (process (cadr parsed-exp))
         (pop_scope_from_environment)
         (when (and (not (has_returned)) (process (caddr parsed-exp)))
           (loop))))]
    [(eq? (car parsed-exp) 'for-exp)
     (push_scope_to_environment '())
     (process (cadr parsed-exp))
     (let loop ()
       (when (and (not (has_returned)) (process (caddr parsed-exp)))
         (process (list-ref parsed-exp 4))
         (unless (has_returned)
           (process (cadddr parsed-exp))
           (loop))))
     (pop_scope_from_environment)]
    [(eq? (car parsed-exp) 'return-exp)
     (wolf_env environment '|#return| (process (cadr parsed-exp)))]
    [(eq? (car parsed-exp) 'fire-exp)
     (displayln (process (cadr parsed-exp)))]
    [(eq? (car parsed-exp) 'new-func-exp) parsed-exp]
    [(eq? (car parsed-exp) 'call-exp)
     ; Evaluate the arguments BEFORE entering the function's new scope.
     (let* ([func (process (cadr parsed-exp))]
            [parameters (cadr func)]
            [arguments (map process (caddr parsed-exp))]
            [new_scope (map (lambda (p a) (list (cadr p) a))
                            parameters arguments)])
       (unless (= (length parameters) (length arguments))
         (error 'call "wrong number of function arguments"))
       (push_scope_to_environment
        (cons (list '|#return| '|#none|) new_scope))
       (process (caddr func))
       (let ([result (resolve_env environment '|#return|)])
         (pop_scope_from_environment)
         (if (eq? result '|#none|) (void) result)))]
    [else (error 'process "unsupported expression: ~a" parsed-exp)]))

(provide (all-defined-out))
