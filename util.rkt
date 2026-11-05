#lang racket

; Variable scopes and environment (professor's week10d2 interface)
(define scope (list (list 'a 1) (list 'b 2)))
(define environment (list scope))

(define (resolve var_scope var_name)
  (cond [(null? var_scope) (void)]
        [(eq? (caar var_scope) var_name) (cadar var_scope)]
        [else (resolve (cdr var_scope) var_name)]))

(define (resolve_env var_env var_name)
  (cond [(null? var_env) (void)]
        [(void? (resolve (car var_env) var_name))
         (resolve_env (cdr var_env) var_name)]
        [else (resolve (car var_env) var_name)]))

(define (update_pair left_part lst key value)
  (cond [(null? lst) left_part]
        [(eq? (caar lst) key)
         (append left_part (list (list key value)) (cdr lst))]
        [else (update_pair (append left_part (list (car lst)))
                           (cdr lst) key value)]))

(define (update_scope var_scope var_name var_value)
  (if (void? (resolve var_scope var_name))
      (cons (list var_name var_value) var_scope)
      (update_pair '() var_scope var_name var_value)))

; Insert at the current scope if missing; otherwise update the nearest scope.
(define (wolf_env var_env var_name var_value)
  (define (change scopes)
    (cond [(null? scopes) '()]
          [(not (void? (resolve (car scopes) var_name)))
           (cons (update_scope (car scopes) var_name var_value)
                 (cdr scopes))]
          [else (cons (car scopes) (change (cdr scopes)))]))
  (set! environment
        (if (void? (resolve_env var_env var_name))
            (cons (update_scope (car var_env) var_name var_value)
                  (cdr var_env))
            (change var_env))))

(define (push_scope_to_environment new_scope)
  (set! environment (cons new_scope environment)))

(define (pop_scope_from_environment)
  (set! environment (cdr environment)))

(define (update_base_environment new_env)
  (set! environment new_env))

(define (is_valid_math_op op)
  (and (symbol? op) (memq op '(+ - * / // %)) #t))

(define (is_valid_boolean_op op)
  (and (symbol? op) (memq op '(> >= < <= == && ||)) #t))

(define (do_math op left right)
  (case op
    [(+) (+ left right)] [(-) (- left right)]
    [(*) (* left right)] [(/) (/ left right)]
    [(//) (quotient left right)] [(%) (modulo left right)]))

(define (calculate_boolean op left right)
  (case op
    [(>) (> left right)] [(>=) (>= left right)]
    [(<) (< left right)] [(<=) (<= left right)]
    [(==) (equal? left right)]
    [(&&) (and left right)] [(||) (or left right)]))

; Preserve parameter pairing order.
(define (create_pair_list lst_ret lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      lst_ret
      (create_pair_list (cons (list (car lst1) (car lst2)) lst_ret)
                        (cdr lst1) (cdr lst2))))

(provide (all-defined-out))
