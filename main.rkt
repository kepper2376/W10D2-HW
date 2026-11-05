#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(process (parse '(
  main
  (a = 1)
  (b = 2)
  (func add (a b) (
    (c = (a + b))
    (return c)
  ))
  (c = (call add (a b)))
  (fire c)
)))

;### homework: greatest common divisor
;write gcd with (Euclid's algorithm) in our defined language in two ways:
;  1. loop manner: use while (or do-while / for) to repeat the Euclid step
;  2. recursive manner: the function calls itself
;test 3 cases for each way, for example:
;  gcd(48, 18) = 6
;  gcd(17, 5) = 1
;  gcd(100, 75) = 25
;write your code below

; Loop version of Euclid's algorithm
(process (parse '(
  main
  (func gcd_loop (x y) (
    (while (y > 0) (
      (temp = (x % y))
      (x = y)
      (y = temp)
    ))
    (return x)
  ))
  (fire (call gcd_loop (48 18)))   ; Expected: 6
  (fire (call gcd_loop (17 5)))    ; Expected: 1
  (fire (call gcd_loop (100 75)))  ; Expected: 25
)))

; Recursive version of Euclid's algorithm
(process (parse '(
  main
  (func gcd_recursive (x y) (
    (ask (y == 0)
      ((return x))
      ((return (call gcd_recursive (y (x % y)))))
    )
  ))
  (fire (call gcd_recursive (48 18)))   ; Expected: 6
  (fire (call gcd_recursive (17 5)))    ; Expected: 1
  (fire (call gcd_recursive (100 75)))  ; Expected: 25
)))
