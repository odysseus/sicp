#lang racket
; Project Euler Problem 1
(define (eu1)
  (define (tracker total n)
    (if (< n 1000)
    (if (or (= (modulo n 3) 0) (= (modulo n 5) 0))
        (tracker (+ total n) (+ n 1))
        (tracker total (+ n 1)))
    total))
  (tracker 0 3))

; Finding the nth number in the Fibonacci sequence
; through tail end recursion
(define (fibo n)
  (define (fibointerior a b count final)
    (if (= count final)
        b
        (fibointerior b (+ a b) (+ count 1) final)))
  (fibointerior 0 1 0 n))

; Project Euler Problem 2
(define (eu2)
  (define (tracker total fibon)
    (if (< (fibo (+ fibon 3)) 4000000)
        (tracker (+ total (+ (fibo (+ fibon 3)))) (+ fibon 3))
        total))
  (tracker 2 2))

(define (eu31)
  (define (british-count-change amount)
    (define (first-denomination kinds-of-coins)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 2)
            ((= kinds-of-coins 3) 5)
            ((= kinds-of-coins 4) 10)
            ((= kinds-of-coins 5) 20)
            ((= kinds-of-coins 6) 50)
            ((= kinds-of-coins 7) 100)
            ((= kinds-of-coins 8) 200)))
    (define (cc amount kinds-of-coins)
      (cond ((= amount 0) 1)
            ((or (< amount 0) (= kinds-of-coins 0)) 0)
            (else (+ (cc amount
                         (- kinds-of-coins 1))
                     (cc (- amount
                            (first-denomination kinds-of-coins))
                         kinds-of-coins)))))
    (cc amount 8))
  (british-count-change 200))







