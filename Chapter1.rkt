#lang racket

; Basics

; variable declarations
(define size 2)
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))

; basic named procedures
(define (square x) (* x x))
(square 5)

(define (sum-of-squares x y) 
  (+ (square x) (square y)))

(sum-of-squares 4 3)

; conditional analysis
; three ways of writing the same function
(define (absolute x)
  (cond ((> 0 x) x)
        ((< 0 x) (- x))
        ((= x 0) 0)))

(define (absval x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (not (< x y)))

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))

; Finding the square root of a number
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (average x y) (/ (+ x y) 2))
  (sqrt-iter 1.0))

; Finding the nth number in the Fibonacci sequence
; through tail end recursion
(define (fibo n)
  (define (fibointerior a b count final)
    (if (= count final)
        b
        (fibointerior b (+ a b) (+ count 1) final)))
  (fibointerior 0 1 0 n))

; Recursive Factorial
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

; Tail End Recursive Factorial
(define (fac n)
  (define (infact n total)
    (if (= n 1)
        total
        (infact (- n 1) (* total n))))
  (infact (- n 1) n))

; Iterative Factorial
(define (iterfact n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))

; Ackerman's Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Recursive Fibonacci Method
(define (fibr n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibr (- n 1))
                 (fibr (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; counting the ways to make change with n coins
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Exercise 1.11
(define (fn n)
  (if (< n 3)
      n
      (+ (fn (- n 1)) (* (fn (- n 2)) 2) (* (fn (- n 3)) 3))))

; Basic recursive exponents
(define (pow n x)
  (if (= x 0)
      1
      (* n (pow n (- x 1)))))

(define (exp b n)
  (define (exp-iter b counter prod)
    (if (= counter 0)
        prod
        (exp-iter b (- counter 1) (* b prod))))
  (exp-iter b n 1))

; Using sucessive squaring gives this (logN) growth
; because fewer operations are performed
(define (fast-pow b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-pow b (/ n 2))))
        (else (* b (fast-pow b (- n 1))))))

(define (even? n) (= (remainder n 2) 0))

; lets assume the language could only do addition and
; subtraction, you could do multiplication/division
; by repeated addition/subtraction
(define (times a b)
  (if (= b 0)
      0
      (+ a (times a (- b 1)))))

(define (double n) n + n)
(define (halve n) (/ n 2))

(define (fibo n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))))))








